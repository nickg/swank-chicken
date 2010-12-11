;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SWANK server
;;;
;;; This section provides the plumbing to allow SLIME to tunnel requests into
;;; the swank:* functions in the next section which do the useful work.
;;;
;;; To start a server do:
;;;   (swank-server-start)
;;;
;;; Then connect to it from Emacs using:
;;;   M-x slime-connect
;;; And accept the default options.
;;;

(require 'tcp)
(require 'posix)
(require-extension symbol-utils)

;; When SWANK commands are evaluated the output port is bound to a special
;; callback that sends the strings back to SLIME for printing. This function
;; produces a function that when executed always uses the default output
;; port. This means we can print debug messages to stdout rather than
;; sending them back to SLIME. Any function that can be called from a
;; swank:* function should be wrapped in this.
(define swank-with-normal-output-port-lambda
  (let ((normal-port (current-output-port)))
    (lambda (func)
      (lambda args
        (with-output-to-port normal-port
          (lambda ()
            (apply func args)))))))

;; Send list `msg' as a reply back to SLIME.
(define swank-write-packet
  (swank-with-normal-output-port-lambda
   (lambda (msg out)
     (define (pad-hex-string n pad)
       (let* ((base (format "~x" n))
              (length (string-length base)))
         (if (>= length pad)
             base
             (string-append (make-string (- pad length) #\0)
                            base))))

     (let* ((string (with-output-to-string
                      (lambda ()
                        (write msg))))
            (padded (pad-hex-string (string-length string) 6))
            (packet (string-append padded string)))

       ;; This may be called by code that has set print-length-limit so we
       ;; have to use this kludge to avoid truncating the packet.
       (##sys#with-print-length-limit #f (lambda ()
                                           (print (format "WRITE ~a" packet))
                                           (display packet out)))
       
       (flush-output out)))))

;; Tail-recursive loop to read commands from SWANK socket and dispatch them.
;; Several calls to this may be active at once e.g. when using the debugger.
(define (swank-event-loop in out)
  (let* ((length (read in))
         (request (read in)))
    (cond
     ((or (eof-object? length) (eof-object? request))
      (void))
     (else
      (print (format "READ ~a ~a" length request))
      (case (car request)
        ((:emacs-rex) (apply swank-emacs-rex in out (cdr request))))
      (swank-event-loop in out)))))

;; Format the call chain for output by SLIME.
(define (swank-call-chain chain)
  (define (frame-string f)
    (format "~a ~a ~s"
            (vector-ref f 0)
            (vector-ref f 1)
            (vector-ref f 2)))
  
  (define (loop n frames)
    (cond
     ((null? frames) '())
     (else (cons (list n (frame-string (car frames)))
                 (loop (add1 n) (cdr frames))))))

  (loop 0 (drop (reverse chain) 1)))

;; Called when an exception is thrown while evaluating a swank:* function.
(define (swank-exception in out id exn chain)
  (define (format-list items)
    (string-concatenate (map (lambda (item) (format "~a " item)) items)))
  
  (let ((get-key (lambda (key)
                   ((condition-property-accessor 'exn key) exn))))
    (print (format "ERROR msg: ~a args: ~a loc ~a"
                   (get-key 'message)
                   (get-key 'arguments)
                   (get-key 'location)))
    (let ((first-line (format "Error: ~a: ~a"
                              (get-key 'message)
                              (format-list (get-key 'arguments))))
          (second-line (cond
                        ((get-key 'location) => (lambda (l)
                                                  (format "  ~a" l)))
                        (else ""))))
      (swank-write-packet
       `(:debug 0 0        ; Thread, level (dummy values)
                (,first-line ,second-line nil)            ; Condition
                (("ABORT" "Return to SLIME's top level")) ; Restarts
                ,(swank-call-chain chain)                 ; Frames
                (,id))     ; Emacs continuations
       out))
    (dynamic-wind
      (lambda ()
        (swank-write-packet '(:debug-activate 0 0 nil) out))
      (lambda ()
        (swank-event-loop in out))
      (lambda ()
        (swank-write-packet '(:debug-return 0 0 nil) out)))))

;; Create an output port that sends data back to SLIME to be printed on
;; the REPL.
(define (swank-output-port socket)
  (make-output-port
   (lambda (str)
     (swank-write-packet `(:write-string ,(format "~a" str))
                         socket))
   void
   void))

;; Evaluate an S-expression and returns a pair (condition . trace) if an
;; exception is raised or (#t . value) on success.
(define (swank-eval-or-condition sexp)
  (call-with-current-continuation
   (lambda (skip)
     (with-exception-handler
      (lambda (exn)
        (let ((chain (get-call-chain)))
          (skip (cons exn chain))))
      (lambda ()
        (cons #t (eval sexp)))))))

;; The only SWANK command we handle at the moment. Argument is an expression
;; to be evaluated, which should be a call to a swank:* function. Any output
;; produced by the expression will be returned to SLIME as a string. Any
;; error generated by the expression will be caught in the exception handler
;; above.
(define (swank-emacs-rex in out sexp package thread id)
  (let ((result `(:return (:abort nil) ,id)))
    (dynamic-wind
      void
      
      (lambda ()
        (let ((thing (with-output-to-port (swank-output-port out)
                       (lambda ()
                         (swank-eval-or-condition sexp)))))
          (cond
           ((condition? (car thing))
            (swank-exception in out id (car thing) (cdr thing)))
           (else
            (set! result `(:return ,(cdr thing) ,id))))))

      ;; Output is always written when unwinding the stack to ensure we
      ;; reply to every message e.g. when escaping via continuation to
      ;; the top level after exiting the debugger.
      (lambda ()
        (swank-write-packet result out)))))

;; The top level continuation. By invoking this we can jump out of the
;; debugger and get back to the REPL.
(define *swank-top-level* #f)

;; Start up a TCP server, wait for a connection, then jump into the main
;; event loop. If `file' is specified it is used as a filename to write
;; the server port number into. This is polled by SLIME to see if the
;; SWANK server has started. Use `swank-server-start' rather than calling
;; this directly.
(define (swank-make-server port file)
  (define (write-port-file port file)
    (call-with-output-file file
      (lambda (handle)
        (write port handle))))

  (tcp-read-timeout #f)

  (let ((listener (tcp-listen port)))
    (if file
        (write-port-file port file))
    (dynamic-wind
      (lambda () #f)
      
      (lambda ()
        (call-with-values
            (lambda () (tcp-accept listener))
          (lambda (in out)
            (call/cc (lambda (hop)
                       (set! *swank-top-level* hop)))
            ;; Whenever we escape from the debugger we'll end up here
            (swank-event-loop in out))))
      
      (lambda ()
        (tcp-close listener)))))

;; Wrapper for `swank-make-server' with optional `port' and `file' arguments.
(define swank-server-start
  (case-lambda
    (() (swank-make-server 4005 #f))
    ((port) (swank-make-server port #f))
    ((port file) (swank-make-server port file))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SWANK commands
;;;
;;; The functions in this section implement the SWANK API proper.
;;;

;; Called at startup to determine various information about the inferior
;; Lisp process.
(define (swank:connection-info)
  `(:ok (:pid ,(current-process-id)
         :package (:name CSI :prompt CSI)
         :lisp-implementation 
         (:type "Chicken Scheme" :version ,(chicken-version)))))

;; For us this call is fairly pointless, but it names the REPL.
(define (swank:create-repl _)
  '(:ok ("CSI" "CSI")))

;; Parse `str' into a list of forms.
(define (string->forms str)
  (define (get-forms)
    (let ((form (read)))
      (cond
       ((eof-object? form) '())
       (else (cons form (get-forms))))))       

  (with-input-from-string str get-forms))

;; Evaluate `str' as if enclosed in (begin ...) and return the results.
(define (swank:listener-eval str)
  (call-with-values
      (lambda ()
        (let ((forms (string->forms str)))
          (if (not (null? forms))
              (eval `(begin ,@forms)))))
    (lambda results
          `(:ok (:values ,@(map (lambda (r)
                                  (format "~a" r))
                                results))))))

;; "Compile" a string. For us this just means eval and discard the
;; results, the behaviour of which might be different to what SLIME
;; or the user expected.
(define (swank:compile-string-for-emacs str buffer position filename _)
  (let ((forms (string->forms str)))
    (for-each (lambda (form)
                (eval form))
              forms)
    `(:ok (:compilation-result nil t 0.0 nil nil))))  ; TODO: eval time

;; Given a function name return a list of its arguments. This uses
;; the symbol-utils extension.
(define (swank:operator-arglist func _)
  `(:ok ,(let ((sym (string->symbol func)))
           (cond
            ((unbound? sym) 'nil)
            ((procedure? (symbol-value sym))
             (format "~a" (procedure-information (symbol-value sym))))
            (else 'nil)))))

;; Immediately return from all nested debugging sessions back to
;; the top level.
(define (swank:throw-to-toplevel)
  (*swank-top-level* (void)))

;; Invoke the given debug restart. We only support one restart at
;; the moment which jumps back to the top level.
(define (swank:invoke-nth-restart-for-emacs _ _)
  (*swank-top-level* (void)))

;; Return the backtrace frames between `start' and `end'.
;; TODO: currently does nothing as we only seem to get eight stack
;; frames from get-call-chain.
(define (swank:backtrace start end)
  '(:ok nil))

;; Load the specified file.
(define (swank:load-file file)
  (load file)
  `(:ok t))

;; Definitions required for CL compatibility.
(define nil #f)
(define t #t)

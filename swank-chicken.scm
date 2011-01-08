;;;
;;; Copyright (c) 2010-2011 Nick Gasson
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;

(require 'tcp)
(require 'posix)
(require-extension symbol-utils
                   apropos
                   chicken-doc
                   fmt)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
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

;; Safe printing routine that always uses the server's standard output.
(define debug-print
  (let ((normal-port (current-output-port)))
    (lambda args
      (with-output-to-port normal-port
        (lambda ()
          (apply print args)))
      (flush-output normal-port))))

;; Send list `msg' as a reply back to SLIME.
(define (swank-write-packet msg out)
  (let* ((string (with-output-to-string
                   (lambda ()
                     (write msg))))
         (pad-hex (lambda (n)
                    (pad-char #\0 (pad/left 6 (num n 16)))))
         (packet (fmt #f (pad-hex (string-length string)) string)))
                      
    ;; This may be called by code that has set print-length-limit so we
    ;; have to use this kludge to avoid truncating the packet.
    (##sys#with-print-length-limit #f (lambda ()
                                        (debug-print (fmt #f "WRITE " (wrt packet)))
                                        (display packet out)))
       
    (flush-output out)))

;; Replace Common Lisp-isms such as :foo, nil, and t with (quote foo),
;; (quote ()), and #t respectively.
(define (swank-cleanup sexp)
  (define (cleanup-symbol sym)
    (let ((sym-str (symbol->string sym)))
      (cond
       ((eq? (string-ref sym-str 0) #\:)
        `(quote ,(string->symbol (substring sym-str 1))))
       ((eq? sym 'nil)
        '(quote ()))
       ((eq? sym 't)
        #t)
       (else sym))))
  
  (cond
   ((list? sexp)
    (map swank-cleanup sexp))
   ((symbol? sexp)
    (cleanup-symbol sexp))
   (else sexp)))

;; Tail-recursive loop to read commands from SWANK socket and dispatch them.
;; Several calls to this may be active at once e.g. when using the debugger.
(define (swank-event-loop in out)
  (let* ((length (read in))
         (request (read in)))
    (cond
     ((or (eof-object? length) (eof-object? request))
      (void))
     (else
      (debug-print (fmt #f "READ " (wrt length) (wrt request)))
      (case (car request)
        ((:emacs-rex)
         (begin
           (apply swank-emacs-rex in out (swank-cleanup (cdr request)))
           (swank-event-loop in out)))
        ((:emacs-return-string)
         (cadddr request)))))))

;; Format the call chain for output by SLIME.
(define (swank-call-chain chain)
  (define (frame-string f)
    (fmt #f (pad 9 (dsp (vector-ref f 0)))
         (pad 16 (dsp (cond
                       ((vector-ref f 2) => (lambda (where)
                                              (fmt #f "[" where "]")))
                       (else ""))))
         " "
         (dsp (vector-ref f 1))))
  
  (define (loop n frames)
    (cond
     ((null? frames) '())
     (else (cons (list n (frame-string (car frames)))
                 (loop (add1 n) (cdr frames))))))

  (loop 0 (drop (reverse chain) 1)))

;; Called when an exception is thrown while evaluating a swank:* function.
(define (swank-exception in out id exn chain)
  (let ((get-key (lambda (key)
                   ((condition-property-accessor 'exn key '()) exn))))
    (debug-print (fmt #f "ERROR msg: " (get-key 'message)
                      " args: " (get-key 'arguments)
                      " loc " (get-key 'location)))
    (let ((first-line (fmt #f "Error: "
                              (cond
                               ((get-key 'location) => (lambda (l)
                                                         (fmt #f "(" l ") ")))
                               (else ""))
                              (get-key 'message)
                              ": " (fmt-join wrt (get-key 'arguments) " "))))
      (swank-write-packet
       `(:debug 0 0        ; Thread, level (dummy values)
                (,first-line "" nil)                      ; Condition
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
(define (swank-output-port out)
  (make-output-port
   (lambda (str)
     (swank-write-packet `(:write-string ,(fmt #f str))
                         out))
   void
   void))

;; Create an input port that reads data by calling back into Emacs.
(define (swank-input-port in out) 
  (let ((buffer #f))
    (define (read-callback)
      (if buffer
          (let ((char (read-char buffer)))
            (if (eof-object? char)
                (begin
                  (set! buffer #f)
                  (read-callback))
                char))
          (begin
            (swank-write-packet '(:read-string 0 1) out)
            (let ((result (swank-event-loop in out)))
              (if (string? result)
                  (begin
                    (set! buffer
                          (with-input-from-string result current-input-port))
                    (read-callback))
                  #!eof)))))

    (define (ready-callback) buffer)
    
    (make-input-port read-callback ready-callback void)))

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
                         (with-input-from-port (swank-input-port in out)
                           (lambda ()
                             (swank-eval-or-condition sexp)))))))
          (cond
           ((not (condition? (car thing)))
            (set! result `(:return ,(cdr thing) ,id)))
           (((condition-predicate 'exn) (car thing))
            (swank-exception in out id (car thing) (cdr thing)))
           (((condition-predicate 'user-interrupt) (car thing))
            (set! result `(:return (:abort user-interrupt) ,id))))))
           
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

            (let ((orig-handler (current-exception-handler)))
              (with-exception-handler
               (lambda (exn)
                 (cond
                  (((condition-predicate 'user-interrupt) exn)
                   (*swank-top-level* (void)))
                  (else 
                   (orig-handler exn))))
               (lambda ()
                 (swank-event-loop in out)))))))
      
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
                                  (fmt #f (wrt r)))
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

;; Evaluate an expression and return a result for the minibuffer.
(define (swank:interactive-eval str)
  (let* ((forms (string->forms str))
         (result (if (not (null? forms))
                     (fmt #f "=> " (eval `(begin ,@forms)))
                     "; No value")))
    `(:ok ,result)))

(define swank:interactive-eval-region swank:interactive-eval)

;; Evaluate an expression and pretty-print the result.
(define (swank:pprint-eval str)
  `(:ok ,(fmt #f (pretty (eval `(begin ,@(string->forms str)))))))

;; Given a function name return a list of its arguments. This uses
;; the symbol-utils extension.
(define (swank:operator-arglist func _)
  `(:ok ,(let ((sym (string->symbol func)))
           (cond
            ((unbound? sym) 'nil)
            ((procedure? (symbol-value sym))
             (fmt #f (procedure-information (symbol-value sym))))
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

;; A more advanced version of swank:operator-arglist which highlights
;; the the current cursor position in the argument list.
(define (swank:autodoc forms . args)

  (define (find-cursor thing)
    (cond
     ((and (list? thing)
           (memq 'swank::%cursor-marker% thing))
      thing)
     ((list? thing)
      (let loop ((elems thing))
        (if (null? elems)
            #f
            (or (find-cursor (car elems))
                (loop (cdr elems))))))
     (else #f)))
  
  (define (highlight-arg info args)
    (cond
     ((null? info) '())
     ((null? args) info)
     ((not (pair? info))  ; Variable length argument list
      (list '===> info '<===))  
     ((eq? (car args) 'swank::%cursor-marker%)
      (append (list '===> (car info) '<===)
              (cdr info)))
     ((and (string? (car args)) (string=? (car args) ""))
      (highlight-arg info (cdr args)))
     (else (cons (car info)
                 (highlight-arg (cdr info) (cdr args))))))
  
  (define (info sym)
    (cond
     ((unbound? sym) #f)
     ((procedure? (symbol-value sym))
      (let ((pi (procedure-information (symbol-value sym))))
        (if (pair? pi)
            pi
            `(,pi . args))))
     (else #f)))
  
  (let ((where (find-cursor forms)))
    (if (and where (string? (car where)))
        (let ((i (info (string->symbol (car where)))))
          (if i
              `(:ok ,(fmt #f (highlight-arg i where)))
              `(:ok :not-available)))
        '(:ok :not-available))))

;; Return a list of all symbols that start with `prefix'.
(define (swank:simple-completions prefix _)
  (let ((comps (filter (lambda (str)
                         (string-prefix? prefix str))
                       (map (lambda (info)
                              (symbol->string (car info)))
                            (apropos-information-list prefix)))))
  `(:ok (,comps ,(if (= (length comps) 1)
                     (car comps)
                     prefix)))))

(define (swank:describe-symbol sym)
  `(:ok ,(with-output-to-string
           (lambda ()
             (doc-dwim sym)))))

(define (swank:describe-definition-for-emacs sym type)
  (swank:describe-symbol sym))

(define (swank:apropos-list-for-emacs str . _)
  (define (slime-node-type node)
    (case (node-type node)
      ((procedure) ':function)
      ((read syntax) ':macro)
      ((setter) ':setf)
      ((class) ':class)
      ((method) ':generic-function)
      ((egg) ':egg)   ; Not visible
      (else ':variable)))
  
  `(:ok ,(map (lambda (node)
                (list ':designator (fmt #f (node-id node))
                      (slime-node-type node) (node-signature node)))
              (match-nodes (irregex str)))))

;; Unimplemented.
(define (swank:buffer-first-change . _) '(:ok nil))
(define (swank:filename-to-modulename . _) '(:ok nil))
(define (swank:find-definitions-for-emacs . _) '(:ok nil))
(define (swank:swank-require . _) '(:ok nil))

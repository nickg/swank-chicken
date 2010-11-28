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

(define (swank-event-loop in out)
  (let* ((length (read in))
         (request (read in)))
    (cond
     ((or (eof-object? length) (eof-object? request))
      (void))
     (else
      (print (format "READ ~a ~a" length request))
      (case (car request)
        ((:emacs-rex) (apply swank-emacs-rex out (cdr request))))
      (swank-event-loop in out)))))

;; When SWANK commands are evaluated the output port is bound to a special
;; callback that sends the strings back to SLIME for printing. This function
;; produces a function that when executed always uses the default output
;; port. This means we can print debug messages to stdout rather than
;; sending them back to SLIME.
(define swank-with-normal-output-port-lambda
  (let ((normal-port (current-output-port)))
    (lambda (func)
      (lambda args
        (with-output-to-port normal-port
          (lambda ()
            (apply func args)))))))
           
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

     (let* ((string (format "~a" msg))
            (packet (format "~a~a"
                            (pad-hex-string (string-length string) 6)
                            string)))
       (print (format "WRITE ~a" packet))
       (display packet out)
       (flush-output out)))))

(define swank-exception
  (swank-with-normal-output-port-lambda
   (lambda (exn)
     (let ((get-key (lambda (key)
                      ((condition-property-accessor 'exn key) exn))))
       (print (format "ERROR msg: ~a args: ~a loc ~a"
                      (get-key 'message)
                      (get-key 'arguments)
                      (get-key 'location)))))))
  
(define (swank-emacs-rex out sexp package thread id)
  (let ((result (call-with-current-continuation
                 (lambda (k)
                   (with-exception-handler
                    (lambda (exn)
                      (swank-exception exn)
                      (k '(:abort nil)))
                    (lambda ()
                      (with-output-to-port (swank-output-port out)
                        (lambda ()
                          (list ':ok (eval sexp))))))))))
    (swank-write-packet (list ':return result id) out)))

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
            (swank-event-loop in out))))
      
      (lambda ()
        (tcp-close listener)))))

(define (swank-output-port socket)
  (make-output-port
   (lambda (str)
     (swank-write-packet `(:write-string ,(format "~s" str))
                         socket))
   void
   void))

(define swank-server-start
  (case-lambda
    (() (swank-make-server 4005 #f))
    ((port) (swank-make-server port #f))
    ((port file) (swank-make-server port file))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SWANK commands

(define (swank:connection-info)
  `(:pid ,(current-process-id)
    :package (:name "CSI" :prompt "CSI")))

(define (swank:create-repl _)
  (list "CSI" "CSI"))

(define (swank:listener-eval sexp)

  (define (get-forms)
    (let ((form (read)))
      (cond
       ((eof-object? form) '())
       (else (cons form (get-forms))))))       

  (with-input-from-string sexp
    (lambda ()
      (call-with-values
          (lambda ()
            (let ((forms (get-forms)))
              (if (not (null? forms))
                  (eval `(begin ,@forms)))))
        (lambda results
          `(:values ,@(map (lambda (r)
                             (format "\"~a\"" r))
                           results)))))))

;; Definitions required for CL compatility
(define nil #f)
(define t #t)

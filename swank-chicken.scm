;;; SWANK server

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
           
(define (swank-write-packet msg out)
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
    (print "WRITE " packet)
    (display packet out)
    (flush-output out)))

(define (swank-exception exn)
  (let ((get-key (lambda (key)
                   ((condition-property-accessor 'exn key) exn))))
    (print (format "Error: msg: ~a args: ~a loc ~a"
                   (get-key 'message)
                   (get-key 'arguments)
                   (get-key 'location)))))
  
(define (swank-emacs-rex out sexp package thread id)
  (let ((result (call-with-current-continuation
                 (lambda (k)
                   (with-exception-handler
                    (lambda (exn)
                      (swank-exception exn)
                      (k '(:abort nil)))
                    (lambda ()
                      (list ':ok (eval sexp))))))))
    (swank-write-packet (list ':return result id) out)))
        
(define (swank-server-start)
  (let ((listener (tcp-listen 4005)))
    (dynamic-wind
      (lambda () #f)
      
      (lambda ()
        (call-with-values
            (lambda () (tcp-accept listener))
          (lambda (in out)
            (swank-event-loop in out))))

      (lambda ()
        (tcp-close listener)))))

(tcp-read-timeout #f)

;;; SWANK commands

(define (swank:connection-info)
  `(:pid ,(current-process-id)
    :package (:name "CSI" :prompt "CSI")))

(define (swank:create-repl _)
  (list "CSI" "CSI"))

(define (swank:listener-eval sexp)
  (with-input-from-string sexp
    (lambda ()
      `(:values ,(format "\"~a\"" (eval (read)))))))

;; Definitions required for CL compatility
(define nil #f)
(define t #t)

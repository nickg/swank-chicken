(define (swank-event-loop in out)
  (let* ((length (read in))
         (request (read in)))
    (cond
     ((or (eof-object? length) (eof-object? request))
      (void))
     (else
      (print (format "~a ~a" length request))
      (case (car request)
        ((:emacs-rex) (apply swank-emacs-rex out (cdr request))))
      (swank-event-loop in out)))))

(define (swank-exception exn)
  (let ((get-key (lambda (key)
                   ((condition-property-accessor 'exn key) exn))))
    (print (format "Error: msg: ~a args: ~a loc ~a"
                   (get-key 'message)
                   (get-key 'arguments)
                   (get-key 'location)))))
  

(define (swank-emacs-rex out sexp package thread id)
  (call-with-current-continuation
   (lambda (k)
     (with-exception-handler
      (lambda (exn)
        (swank-exception exn)
        (k exn))
      (lambda ()
        (eval sexp))))))

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

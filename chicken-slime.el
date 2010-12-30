(setq slime-lisp-implementations
      (cons '(chicken ("csi")
                      :init chicken-slime-init)
            slime-lisp-implementations))

(defvar swank-chicken-path "/path/to/swank-chicken.scm")

(defun chicken-slime-init (file _)
  (setq slime-protocol-version 'ignore)
  (setq slime-complete-symbol-function 'slime-simple-complete-symbol)
  (let ((swank swank-chicken-path))
    (format "%S\n"
            `(begin (load ,swank) 
                    (swank-server-start 4005 ,file)))))

(defun chicken-slime ()
  (interactive)
  (slime "chicken"))

(provide 'chicken-slime)

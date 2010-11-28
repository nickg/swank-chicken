(setq slime-lisp-implementations
      (cons '(chicken ("csi")
                      :init chicken-slime-init)
            slime-lisp-implementations))

(defun chicken-slime-init (file _)
  (setq slime-protocol-version 'ignore)
  (let ((swank "/home/nick/swank-chicken/swank-chicken.scm"))
    (format "%S\n"
            `(begin (require 'tcp)
                    (require 'posix)
                    (require-extension symbol-utils)
                    (load ,swank) 
                    (swank-server-start 4005 ,file)))))

(defun chicken-slime ()
  (interactive)
  (slime "chicken"))

(provide 'chicken-slime)

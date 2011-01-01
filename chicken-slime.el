(setq slime-lisp-implementations
      (cons '(chicken ("csi")
                      :init chicken-slime-init)
            slime-lisp-implementations))

(defvar swank-chicken-path nil
  "Path to swank.scm. Set to nil to use installed extension.")

(defun chicken-slime-init (file _)
  (setq slime-protocol-version 'ignore)
  (setq slime-complete-symbol-function 'slime-simple-complete-symbol)
  
  (format "%S\n"
          `(begin ,@(if swank-chicken-path
                        `((load ,swank-chicken-path)   ; Interpet code for testing
                          (import swank))
                     '((require-extension swank)))     ; Normal use
                  (swank-server-start 4005 ,file))))

(defun chicken-slime ()
  (interactive)
  (slime "chicken"))

(provide 'chicken-slime)

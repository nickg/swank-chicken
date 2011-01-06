(module swank (swank-server-start
               swank:connection-info
               swank:swank-require
               swank:autodoc
               swank:create-repl
               swank:listener-eval
               swank:compile-string-for-emacs
               swank:interactive-eval
               swank:interactive-eval-region
               swank:pprint-eval
               swank:operator-arglist
               swank:throw-to-toplevel
               swank:invoke-nth-restart-for-emacs
               swank:backtrace
               swank:load-file
               swank:autodoc
               swank:simple-completions
               swank:buffer-first-change
               swank:filename-to-modulename
               swank:find-definitions-for-emacs
               swank:swank-require
               swank:describe-symbol
               swank:apropos-list-for-emacs)
  (import scheme
          chicken)
  (use ports
       tcp
       posix
       srfi-1
       srfi-13
       symbol-utils
       apropos
       irregex
       fmt)
  (include "swank-chicken.scm"))

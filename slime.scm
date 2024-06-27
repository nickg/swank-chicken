(module slime (swank-server-start
               swank:connection-info
               swank:swank-require
               swank:autodoc
               swank:create-repl
               swank-repl:create-repl
               swank:listener-eval
               swank-repl:listener-eval
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
               swank:documentation-symbol
               swank:inspect-frame-var
               swank:quit-inspector
               swank:frame-locals-and-catch-tags
               swank:apropos-list-for-emacs
               swank:set-default-directory
               swank:default-directory
               swank:init-presentations
	       swank-trace-dialog:dialog-toggle-trace
	       swank-trace-dialog:dialog-untrace
	       swank-trace-dialog:report-specs
	       swank-trace-dialog:report-total
	       swank-trace-dialog:report-partial-tree
	       swank-trace-dialog:inspect-trace-part
	       swank-trace-dialog:report-trace-detail
	       swank-trace-dialog:clear-trace-tree
	       swank-trace-dialog:dialog-untrace-all
               swank:init-presentations
               swank-repl:create-repl)
  (import scheme
          (chicken base)
          (chicken irregex)
          srfi-1
          srfi-13
          symbol-utils
          apropos
          fmt)
  (include "swank-chicken.scm"))

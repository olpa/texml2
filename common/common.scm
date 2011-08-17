(define (common-transform doc)
  (pre-post-order doc `(
      (*default* *preorder* . ,(lambda (tag . rest)
          (display (string-append "*** Unprocessed tag: " (symbol->string tag) "\n") (current-error-port))
          `(cmd "TODO" (parm ,(symbol->string tag)))))
      (*TOP* . ,(lambda (tag . rest) (cons 'texml rest)))
      )))

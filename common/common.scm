(define sxslt-id (lambda args args))

(define (common-transform doc)
  (pre-post-order doc `(
      (*default* *preorder* . ,(lambda (tag . rest)
          (display (string-append "*** Unprocessed tag: " (symbol->string tag) "\n") (current-error-port))
          `(cmd "TODO" (gr ,(symbol->string tag)))))
      (*TOP* . ,(lambda (tag . rest) (cons 'texml rest)))
      )))

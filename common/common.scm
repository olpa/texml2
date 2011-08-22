(define sxslt-id (lambda args args))
(define sxslt-dummy (lambda args '()))

(define (db tagname)
  (string->symbol (string-append "http://docbook.org/ns/docbook:" tagname)))

(define (common-transform doc)
  (pre-post-order doc `(
      (*default* *preorder* . ,(lambda (tag . rest)
          (display (string-append "*** Unprocessed tag: " (symbol->string tag) "\n") (current-error-port))
          `(cmd "TODO" (gr ,(symbol->string tag)))))
      (*PI* *preorder* . ,(lambda args '()))
      (*text*          . ,(lambda (dummy s) s))
      (@    *preorder* . ,(lambda args '()))
      (,(db "chapter") . ,(lambda (tag . rest) rest))
      (,(db "section") . ,(lambda (tag . rest) rest))
      (,(db "para")    . ,(lambda (tag . rest) `(env "para" ,@rest)))
      (,(db "indexterm") *preorder* . ,sxslt-dummy)
      (,(db "tag")     . ,(lambda (tag . rest) `(cmd "tag" (wr nonl2) (gr ,@rest))))
      (,(db "acronym") . ,(lambda (tag . rest) `(cmd "acro" (wr nonl2) (gr ,@rest))))
      (*TOP* . ,(lambda (tag . rest)
          `(texml
             (cmd "documentclass" (gr "book"))
             (cmd "usepackage" (gr "texml"))
             (env "document"
                  ,@rest))))
      )))

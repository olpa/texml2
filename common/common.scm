(define sxslt-id       (lambda args args))
(define sxslt-flatten  (lambda (tag . args) args))
(define sxslt-drop     (lambda args '()))

(define (db tagname)
  (string->symbol (string-append "http://docbook.org/ns/docbook:" tagname)))

(define (direct-map-inline xml-tag tex-command)
  (cons (db xml-tag)
        (lambda (tag . rest) `(cmd ,tex-command (wr nonl2) (gr ,@rest)))))

(define (common-transform doc)
  (define (sxslt-unknown-tag tag . rest)
    (display (string-append "*** Unprocessed tag: "
	(symbol->string tag) "\n") (current-error-port))
    `(cmd "TODO" (gr ,(symbol->string tag))))
  (pre-post-order doc `(
      (*default* *preorder* . ,sxslt-unknown-tag)
      (*PI* *preorder* . ,sxslt-drop)
      (*text*          . ,sxslt-flatten)
      (@    *preorder* . ,sxslt-drop)
      (,(db "chapter") (
          (,(db "title") . ,(lambda (tag . rest) `(cmd "chapter" (gr ,@rest))))
          )                . ,sxslt-flatten)
      (,(db "section") (
          (,(db "title") . ,(lambda (tag . rest) `(cmd "section" (gr ,@rest))))
          )                . ,sxslt-flatten)
      (,(db "info")        . ,sxslt-flatten)
      (,(db "pubdate")     . ,sxslt-drop)
      (,(db "releaseinfo") . ,sxslt-drop)
      (,(db "para")        . ,(lambda (tag . rest) `(env "para" (wr nonl2 nonl3) ,@rest)))
      (,(db "programlisting")       . ,(lambda (tag . rest) `(env "programlisting" (wr nonl2 nonl3) ,@rest)))
      (,(db "indexterm") *preorder* . ,sxslt-drop)
      ,(direct-map-inline "tag"       "tag")
      ,(direct-map-inline "acronym"   "acronym")
      ,(direct-map-inline "firstterm" "firstterm")
      ,(direct-map-inline "command"   "command")
      (,(db "phrase")        . ,sxslt-flatten)
      (,(db "xref") *preorder*  . ,(lambda args "(TODO-xref)"))
      (*TOP* . ,(lambda (tag . rest)
          `(texml
             (cmd "documentclass" (gr "book"))
             (cmd "usepackage" (gr "texml"))
             (env "document"
                  ,@rest))))
      )))

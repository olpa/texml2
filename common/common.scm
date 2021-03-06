(define sxslt-id       (lambda args args))
(define sxslt-flatten  (lambda (tag . args) args))
(define sxslt-drop     (lambda args '()))

(define (car-sxpath-or-default path default)
  (let ((sx (sxpath path)))
    (lambda (node)
      (let ((ret (sx node)))
        (if (null? ret)
          default
          (car ret))))))

(define (db tagname)
  (string->symbol (string-append "http://docbook.org/ns/docbook:" tagname)))

(define (direct-map-inline xml-tag tex-command)
  `(,(db xml-tag) ((@ *preorder* . ,sxslt-drop)) .
        ,(lambda (tag . rest) `(cmd ,tex-command (wr nonl2) (gr ,@rest)))))
(define (sxslt-unknown-tag tag . rest)
  (display (string-append "*** Unprocessed tag: "
                          (symbol->string tag) "\n") (current-error-port))
  `(cmd "TODO" (gr ,(symbol->string tag))))

(define (get-list-convmap)
  (define list-attr-convmap `(@ (
            (*default* . ,sxslt-drop)
            (spacing . ,(lambda (tag val) (if (string=? "compact" (car val))
                                             '(cmd "CompactVspace"))))
            ) . ,sxslt-flatten))
  (define (mk-listitem numbering) `(,(db "listitem") .
      ,(lambda (tag . rest)
         `(env "listitem" (wr nonl2 nonl3) (gr ,numbering) ,@rest))))
  `((,(db "itemizedlist") (
      ,list-attr-convmap
      ,(mk-listitem '(cmd "tbullet" (wr nonl2)))
      ) . ,(lambda (tag . rest) `(env "itemizedlist" ,@rest)))
    (,(db "orderedlist") (
      ,list-attr-convmap
      ,(mk-listitem '(tx:counter l n +))
      ) . ,(lambda (tag . rest)
        `((tx:counter l #\space push) (tx:counter l #\space =) (env "orderedlist" ,@rest) (tx:counter l #\space pop))))
  )
)

(define (simplilist->table kids type columns)
  (if (= 1 columns)
    (map (lambda (node) (cons node '()))
         (nset-filter (func-is-element-is-named? 'env) kids))
    (raise "simplilist-table, only 1 colun is supported yet")))

(define (common-transform doc)
  (define inquote? #f) ; #f=level0,2,4... #t=level1,3,5...
  (define (lang key) (gentext default-language key))
  (define conv-map `(
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
      (,(db "link") (
          (@ . ,sxslt-flatten)
          (http://www.w3.org/1999/xlink:href . ,(lambda (tag . rest) (cons 'gr rest)))
          )                . ,(lambda (tag . rest)
             (cond
               ((null? rest)       )
               ((null? (cdr rest)) `(cmd "href" (wr nonl2) (gr ) (gr ,@rest)))
               (else               `(cmd "href" (wr nonl2) ,(car rest) (gr ,@(cdr rest)))))))
      (,(db "info")        . ,sxslt-flatten)
      (,(db "pubdate")     . ,sxslt-drop)
      (,(db "releaseinfo") . ,sxslt-drop)
      (,(db "table")       . ,sxslt-flatten)
      (,(db "tgroup") *preorder* . ,(lambda node (convert-table node conv-map)))
      (,(db "para")        . ,(lambda (tag . rest) `(env "para" (wr nonl2 nonl3) ,@rest)))
      (,(db "simpara")     . ,(lambda (tag . rest) `(env "para" (wr nonl2 nonl3) ,@rest)))
      (,(db "member")      . ,(lambda (tag . rest) `(env "para" (wr nonl1 nonl2 nonl3 nonl4) ,@rest)))
      (,(db "note")        . ,(lambda (tag . rest) `(cmd "note" (gr ,@rest))))
      (,(db "programlisting")       . ,(lambda (tag . rest) `(env "programlisting" (wr nonl2 nonl3) ,@rest)))
      (,(db "screen")       . ,(lambda (tag . rest) `(env "screen" (wr nonl2 nonl3) ,@rest)))
      (,(db "variablelist")  . ,(lambda (tag . rest) `(env "variablelist" ,@rest)))
      (,(db "varlistentry") (
                             (,(db "listitem")  . ,(lambda (tag . rest) `(env "varlistitem" (wr nonl1 nonl2 nonl3 nonl4) ,@rest)))
                             )
        . ,(lambda (tag . rest) `(env "varlistentry" (wr nonl2 nonl3) ,@rest)))
      (,(db "term")        . ,(lambda (tag . rest) `(cmd "term" (wr nonl2) (gr ,@rest))))
      (,(db "indexterm") *preorder* . ,sxslt-drop)
      ,(direct-map-inline "tag"       "tag")
      ,(direct-map-inline "acronym"   "acronym")
      ,(direct-map-inline "firstterm" "firstterm")
      ,(direct-map-inline "command"   "command")
      ,(direct-map-inline "uri"       "uri")
      ,(direct-map-inline "replaceable" "replaceable")
      ,(direct-map-inline "citetitle" "citetitle")
      ,(direct-map-inline "remark"    "remark")
      ,(direct-map-inline "emphasis"  "emph")
      ,(direct-map-inline "code"      "code")
      ,(direct-map-inline "computeroutput"  "computeroutput")
      ,(direct-map-inline "literal"   "literal")
      ,(direct-map-inline "filename"  "filename")
      ,(direct-map-inline "function"  "function")
      ,(direct-map-inline "subscript" "textsubscript")
      ,(direct-map-inline "superscript"  "textsuperscript")
      ,(direct-map-inline "footnote"  "footnote")
      (,(db "glossterm") . ,(lambda (tag . rest) rest))
      (,(db "biblioref") *preorder* . ,(lambda self
          (let ((bibref (or (sxml:attr-u self 'linkend) "?")))
            `(cmd "biblioref" (wr nonl2) (gr ,bibref)))))
      (,(db "phrase")        . ,sxslt-flatten)
      (,(db "xref") *preorder*  . ,(lambda args "(TODO-xref)"))
      (,(db "example") *preorder* . ,(lambda (tag . rest)
          (let* ((title "")
                 (title-conv-map (cons (cons (db "title") (lambda (tag . rest) (set! title rest) '())) conv-map))
                 (title-rest (pre-post-order rest title-conv-map)))
            `(env "example" ,@title-rest (cmd "txcaption" (gr ,(lang "Example") " " (tx:counter e n +)) (gr ,@title))))))
      ,@(get-list-convmap)
      (*TOP* . ,(lambda (tag . rest)
          `(texml
             (cmd "documentclass" (gr "book"))
             (cmd "usepackage" (gr "texml"))
             (env "document"
                  ,@rest))))
      (,(db "quote") *preorder* . ,(lambda (tag . rest)
           (set! inquote? (not inquote?))
           (let ((in (pre-post-order rest conv-map))
                 (bq (lang (if inquote? "startquote" "nestedstartquote")))
                 (eq (lang (if inquote? "endquote" "nestedendquote"))))
             (set! inquote? (not inquote?))
             (list bq in eq))))
      (,(db "simplelist") . ,(lambda (tag . kids)
          (let ((type ((car-sxpath-or-default '(@ type *text*) "vert") kids))
                (columns (string->number
                      ((car-sxpath-or-default '(@ columns *text*) "1") kids))))
            (if (string=? "inline" type)
              (raise "simplilist/@type=inline not supported")
              `(env "simplelisttbl"
                  (cmd "TxColwidths" (gr (gr (cmd "p" (wr nonl2) (gr "1")))))
                  ,@(map (lambda (row)
                      `(texml (cmd "brow")
                      ,@(map (lambda (cell)
                                    `(cmd "cell" (gr ,cell)))
                                  row)
                      (cmd "erow")))
                    (simplilist->table kids type columns)))))))
      ;
      ))
  (pre-post-order doc conv-map)
  )

(define tag-drops-xml-space?
  (let ((drop-list (map db (list "chapter" "info" "section" "itemizedlist" "orderedlist" "listitem" "variablelist" "varlistentry" "note" "example" "table" "tgroup" "thead" "tbody" "tfoot" "row"))))
    (lambda (elem-gi)
      (let ((full-gi (if (pair? elem-gi)
                      (string->symbol (string-append
                                        (symbol->string (car elem-gi)) ":"
                                        (symbol->string (cdr elem-gi))))
                      elem-gi)))
        (memq full-gi drop-list)))))

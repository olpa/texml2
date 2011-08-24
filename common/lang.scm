; Languages

(define lang-files-path "support/docbook/gentext/locale")

(define default-language "en")

; Construct the name of the language file and load it
; TODO: error handlers for: IO error, XML error
(define (parse-language-file short-name)
  (define full-name (path-expand (string-append short-name ".xml") lang-files-path))
  (SSAX:XML->SXML (open-input-file full-name) '()))

; The table of translations
; keys: language + '/' + key
(define translations-table '())

; Language file: convert from XML to internal tables
(define (lang-xml-to-table lang doc)
  (define nset ((sxpath '(locale gentext)) doc))
  (for-each (lambda (sxml-gentext)
              (let ((k (sxml:string ((sxpath '(@ key))  sxml-gentext)))
                    (v (sxml:string ((sxpath '(@ text)) sxml-gentext))))
                (set! translations-table (cons
                    (list (string-append lang "/"  k) v) translations-table))))
            nset))

; Load several language files, plus the default one
(define (load-languages llist)
  (let loop ((to-load (cons default-language llist))
             (loaded  '()))
    (if (null? to-load)
      #t
      (let ((cur-lang (car to-load)))
        (if (not (member cur-lang loaded))
          (lang-xml-to-table cur-lang (parse-language-file cur-lang)))
        (loop (cdr to-load) (cons cur-lang loaded))))))

; Translate
(define (gentext lang key)
  (let* ((lang-key (string-append lang "/" key))
         (transl   (assoc lang-key translations-table)))
    (if transl
      (cadr transl)
      (begin
        (cerr "*** No translation for " lang-key #\newline)
        (if (string=? lang default-language)
          ""
          (gentext default-language key))))))

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

;
(define (load-languages llist)
  (let loop ((to-load (cons default-language llist))
             (loaded  '()))
    (if (null? to-load)
      #t
      (let ((cur-lang (car to-load)))
        (if (not (member cur-lang loaded))
          (lang-xml-to-table cur-lang (parse-language-file cur-lang)))
        (loop (cdr to-load) (cons cur-lang loaded))))))

; devel
(define ssax-sxml (getenv "SSAXLIB"))
(load (string-append ssax-sxml "/libs/gambit/common.scm"))
(load (string-append ssax-sxml "/libs/gambit/myenv.scm"))
(load (string-append ssax-sxml "/libs/input-parse.scm"))
(load (string-append ssax-sxml "/libs/srfi-13-local.scm"))
(load (string-append ssax-sxml "/libs/look-for-str.scm"))
(load (string-append ssax-sxml "/ssax/char-encoding.scm"))
(load (string-append ssax-sxml "/ssax/SSAX-code.scm"))
(load (string-append ssax-sxml "/ssax/SXML-tree-trans.scm"))
(load (string-append ssax-sxml "/sxml-tools/xpath-parser.scm"))
(load (string-append ssax-sxml "/sxml-tools/sxpath.scm"))
(load (string-append ssax-sxml "/sxml-tools/sxpathlib.scm"))
(load (string-append ssax-sxml "/sxml-tools/sxpath-ext.scm"))
(load (string-append ssax-sxml "/sxml-tools/txpath.scm"))
;(define doc (parse-language-file "ru"))
;(lang-xml-to-table "ru" doc)
(load-languages '("ru" "ru" "ru"))
(pp translations-table (current-output-port))

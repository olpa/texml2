(define ssax-sxml (getenv "SSAXLIB"))
(load (string-append ssax-sxml "/libs/gambit/common.scm"))
(load (string-append ssax-sxml "/libs/gambit/myenv.scm"))
(load (string-append ssax-sxml "/libs/input-parse.scm"))
(load (string-append ssax-sxml "/libs/srfi-13-local.scm"))
(load (string-append ssax-sxml "/libs/look-for-str.scm"))
(load (string-append ssax-sxml "/libs/util.scm"))
(load (string-append ssax-sxml "/ssax/char-encoding.scm"))
(load (string-append ssax-sxml "/ssax/SSAX-code.scm"))
(load (string-append ssax-sxml "/ssax/SXML-tree-trans.scm"))
(load (string-append ssax-sxml "/sxml-tools/xpath-parser.scm"))
(load (string-append ssax-sxml "/sxml-tools/sxpath.scm"))
(load (string-append ssax-sxml "/sxml-tools/sxpathlib.scm"))
(load (string-append ssax-sxml "/sxml-tools/sxpath-ext.scm"))
(load (string-append ssax-sxml "/sxml-tools/txpath.scm"))
(load (string-append ssax-sxml "/sxml-tools/sxml-tools.scm"))

(load "support/unsorted/convtools.scm")
(load "common/common.scm")
(load "common/xinclude.scm")
(load "common/texml.scm")
(load "common/lang.scm")
(load "common/counter.scm")
(load "common/table.scm")

(define (id-pp obj)
  (display "*** id-pp: " (current-output-port))(pp obj (current-output-port))
  obj)

(define xml-file (cadr (member "--xml" (command-line))))
(define tex-file (cadr (member "--tex" (command-line))))
(define doc (SSAX:XML->SXML (open-input-file xml-file) '()))
;(pp doc (current-output-port))
(define doc (resolve-xincludes doc (path-directory xml-file)))
(load-languages '())
(define texml (common-transform doc))
;(pp texml (current-output-port))
(fix-counters! texml)

(define tex-port (open-output-file tex-file))
(define tex-port-out (make-tex-output tex-port))
(texml-serialize texml tex-port-out)
(close-output-port tex-port)
(exit)

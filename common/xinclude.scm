; A minimal support for a minimal subset of XInclude, enough only
; to process the Definitive Guide. A probably full implementation
; of XInclude can be found in ssax project
;

; No error check for circular includes etc
; Let's hope that the attribute "href" always exists
(define (resolve-xincludes doc basedir)
  (pre-post-order doc `(
      (*text* . ,(lambda (args str) str))
      (http://www.w3.org/2001/XInclude:include (
          (*default*  . ,(lambda (tag . rest)
              (raise (string-append "xinclude, unsupported tag: " tag))))
          (@ *preorder* . ,(lambda (dummy . alist)
                  (let* ((href   (cadr (assoc 'href alist)))
                         (fname  (path-expand href basedir))
                         (fixme  (pp fname))
                         (newdoc (SSAX:XML->SXML (open-input-file fname) '())))
                    (cdr newdoc))))
          )
        . ,(lambda (tag rest) rest))
      (*default* . ,sxslt-id))))

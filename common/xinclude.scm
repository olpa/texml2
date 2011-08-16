; A minimal support for a minimal subset of XInclude, enough only
; to process the Definitive Guide. A probably full implementation
; of XInclude can be found in ssax project
;
; No error check for circular includes etc

(define (resolve-xincludes doc basedir)
  (pre-post-order doc `(
      (*text* . ,(lambda (args str) str))
      (*default* . ,(lambda args args)))))

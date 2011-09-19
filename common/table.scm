; some generic parameters are required
(define (convert-table tgroup-node conv-map)
  (define convmap-ref)
  (define convmap `(
      (,(db "tgroup") *preorder* . ,(lambda node
              ; FIXME: very fragile
              (let* ((ncols (sxml:number
                              ((car-sxpath '(@ cols *text*)) node)))
                     (colwidths
                       (let loop ((i ncols) (cws '()))
                         (if (= 0 i)
                           cws
                           (loop
                             (- i 1)
                             (cons
                               ; colspec[@colnum=i]/@colwidth
                               (parse-colspec-string (sxml:string ((sxpath
                                 `((,(db "colspec")
                                     (@ (equal? (colnum ,(number->string i)))))
                                   @ colwidth)) node)))
                               cws)))))
                     )
                  (pp colwidths)
                  `(env calstable ,@(pre-post-order (cdr node) convmap-ref)))))
      (,(db "thead")  . ,(lambda (_ . kids) `(env thead ,@kids)))
      (,(db "tbody")  . ,(lambda (_ . kids) kids))
      (,(db "row")    . ,(lambda (_ . kids) `((cmd brow (wr nogr)) ,@kids (cmd erow (wr nogr)))))
      (,(db "entry")  . ,(lambda (_ . kids) `(cmd cell (gr ,@kids))))
      ,@conv-map))
  (set! convmap-ref convmap)
  (pre-post-order tgroup-node convmap))

;
; colspec
;

;
; returns (fixed_width_value, fixed_width_units, proportion_sum, percent_sum)
; for example:
; "3Pt + 10% + *"  ==> '("3" "pt" 1 10)
;
;http://www.oasis-open.org/specs/a502.htm
;COLWIDTH: Either proportional measure of the form number*, i.e., "5*" for 5 times the proportion, or "*" (which is equivalent to "1*"); fixed measure, i.e., 2pt for 2 point, 3pi for 3 pica; or mixed measure, i.e., 2*+3pt. Coefficients are positive integers or fixed point numbers.
; Declared Value = CDATA
;Default = IMPLIED (means obtain value from a named style specification if any. If there is no such value, or if the value is "" [the null string] then assume a proportion of "1*".)
;The fixed unit values are case insensitive. The standard list of allowed unit values is "pt" (points), "cm" (centimeters), "mm" (millimeters), "pi" (picas), and "in" (inches). The default fixed unit should be interpreted as "pt" if neither a proportion nor a fixed unit is specified.

; correct strings are parsed correctly. For the rest:
; garbage in -- garbage out.
(define (parse-colspec-string s)
  (let* ((default-result    '(#f #f 1 #f))
         (chars-value       #f)
         (chars-units       #f)
         (fixed-value       #f)
         (fixed-units       #f)
         (percent-value     #f)
         (proportion-value  #f)
         (parse-error       (lambda ls
                              (apply cerr ls)
                              (func-exit default-result)))
         (reset-value       (lambda ()
                              (set! chars-value '())
                              (set! chars-units '())))
         (fix-current-value (lambda ()
            (if (not (null? chars-value))
              (begin
                (set! fixed-value (list->string (reverse chars-value)))
                (set! fixed-units (if (null? chars-units)
                        "pt"
                        (list->string (reverse chars-units))))))))
         )
    (reset-value)
    (let loop ((rest (string->list s)))
      (if (null? rest)
        (begin
          (fix-current-value)
          (list fixed-value fixed-units proportion-value percent-value))
        (let ((ch (car rest)))
          (cond
            ((char-numeric? ch)    (set! chars-value (cons ch chars-value)))
            ((char-lower-case? ch) (set! chars-units (cons ch chars-units)))
            ((char-upper-case? ch) (set! chars-units
                                     (cons (char-downcase ch) chars-units)))
            ((eqv? #\+ ch)         (fix-current-value)
                                   (reset-value))
            ((or (eqv? #\. ch) (eqv? #\, ch))
                    (cond
                      ((null? chars-value)  (set! chars-value '(#\0 #\.)))
                      ((memv #\. chars-value) )
                      (else  (set! chars-value (cons #\. chars-value)))))
            ((eqv? #\* ch)
                    (set! proportion-value
                      (if (null? chars-value)
                        1
                        (string->number (list->string
                                          (reverse chars-value)))))
                    (reset-value))
            ((eqv? #\% ch)
                    (if (not (null? chars-value))
                      (set! percent-value (string->number
                              (list->string (reverse chars-value)))))
                    (reset-value))
            )
          (loop (cdr rest)))))))

;(pp (parse-colspec-string "2 3 C m + 2..5* +  7%%%"))

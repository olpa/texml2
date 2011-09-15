; converting tables
(define (cerr . lst)
  (map (lambda (obj) (display obj)) lst)
  (newline))

; some generic parameters are required
(define (convert-table tgroup-node conv-map)
  (pre-post-order tgroup-node `(
      (,(db "tgroup") . ,(lambda (_ . kids) `(env calstable ,@kids)))
      (,(db "thead")  . ,(lambda (_ . kids) `(env thead ,@kids)))
      (,(db "tbody")  . ,(lambda (_ . kids) kids))
      (,(db "row")    . ,(lambda (_ . kids) `((cmd brow (wr nogr)) ,@kids (cmd erow (wr nogr)))))
      (,(db "entry")  . ,(lambda (_ . kids) `(cmd cell (gr ,@kids))))
      ,@conv-map)))

;
; colspec
;

;
; returns (fixed_width, proportion_sum, percent_sum)
; for example:
; "3pt+10%+*"  ==> '("3pt" 1 10)
;
;(define (parse-colspec-string s)
;
;http://www.oasis-open.org/specs/a502.htm
;COLWIDTH: Either proportional measure of the form number*, i.e., "5*" for 5 times the proportion, or "*" (which is equivalent to "1*"); fixed measure, i.e., 2pt for 2 point, 3pi for 3 pica; or mixed measure, i.e., 2*+3pt. Coefficients are positive integers or fixed point numbers.
; Declared Value = CDATA
;Default = IMPLIED (means obtain value from a named style specification if any. If there is no such value, or if the value is "" [the null string] then assume a proportion of "1*".)
;The fixed unit values are case insensitive. The standard list of allowed unit values is "pt" (points), "cm" (centimeters), "mm" (millimeters), "pi" (picas), and "in" (inches). The default fixed unit should be interpreted as "pt" if neither a proportion nor a fixed unit is specified.

(define (parse-colspec-string s)
  (call-with-current-continuation (lambda (func-exit)
    (let* ((default-result        '(#f #f 1 #f))
           (chars-value           #f)
           (chars-units-or-marker #f)
           (fixed-value           #f)
           (fixed-units           #f)
           (percent-value         #f)
           (proportion-value      #f)
           (parse-error           (lambda ls
                                    (apply cerr ls)
                                    (func-exit default-result)))
           (reset-value           (lambda ()
                                    (set! chars-value '())
                                    (set! chars-unit-or-marker '())))
           (fix-current-value     (lambda () #t))
           )
      (let loop ((rest (string->list s)))
        (if (null? rest)
          (begin
            (fix-current-value)
            (list fixed-value fixed-units proportion-value percent-value))
          (let ((ch (car rest)))
            (cond
              ((eq? ch #\space) (parse-error "*** space" 22))
              (else (loop (cdr rest)))))))
      ))))

(pp (parse-colspec-string "x"))

; counters

; (counter name show action arg)
; name:  a symbol
; show:  how to show.
;        - n: numeric (1, 2, 3, 4, ...)
;        - r: lowercase roman (i, ii, iii, iv, ...)
;        - R: uppercase roman (I, II, III, IV, ...)
;        - a: lowercase alphabetic (a, b, c, d, ...)
;        - A: uppercase alphabetic (A, B, C, D, ...)
;        - #\space: nothing
; action: what to do in addition to show
;        - push: put on stack
;        - pop:  pop from stack
;        - = arg: set the value of the counter to arg
;        - + {arg}: increment the value of the counter, default is 1

(define (fix-counters! tree)
  (let loop ((this-level tree) (parents '()))
    (if (null? this-level)
      (if (null? parents)
        #t
        (loop (cdar parents) (cdr parents)))
      (if (pair? (car this-level))
        (if (eq? (caar this-level) 'tx:counter)
          (begin
            (set-car! (car this-level) 'cccounter)
            (loop (cdr this-level) parents))
          (loop (car this-level) (cons this-level parents)))
        (loop (cdr this-level) parents)))))

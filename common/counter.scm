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
  (define baton '(() ()))
  (let loop ((this-level tree) (parents '()))
    (if (null? this-level)
      (if (null? parents)
        #t
        (loop (cdar parents) (cdr parents)))
      (if (pair? (car this-level))
        (if (eq? (caar this-level) 'tx:counter)
          (begin
            (set-car! this-level (on-counter (car this-level) baton))
            (loop (cdr this-level) parents))
          (loop (car this-level) (cons this-level parents)))
        (loop (cdr this-level) parents)))))

(define (on-counter cnt-node baton)
  (let ((show      (caddr cnt-node))
        (action   (cadddr cnt-node))
        (rest     (cddddr cnt-node))
        (cnt-pair (let ((pair-found (assq (cadr cnt-node) (car baton))))
                    (or pair-found
                      (let ((new-pair (cons (cadr cnt-node) 0)))
                        (set-car! baton (cons new-pair (car baton)))
                        new-pair))))
        )
    (case action
      ((=)   (set-cdr! cnt-pair (if (null? rest) 0 (car rest))))
      ((+)   (set-cdr! cnt-pair (+ (cdr cnt-pair)
                                   (if (null? rest) 1 (car rest)))))
      ; push/pop are balanced, no need for an associative map
      ((push) (set-car! (cdr baton) (cons (cdr cnt-pair) (cadr baton))))
      ((pop)  (set-cdr! cnt-pair    (caadr baton))
              (set-car! (cdr baton) (cdadr baton)))
      (else  (cerr "*** counter fix, unknown action: " action #\newline)))
    ; show modifications are not supported yet
    (if (eq? #\space show)
      '()
      (cdr cnt-pair))))

(define (make-tex-output port)
  (let ((after-weak-nl? #t))
    (lambda args
      (let loop ((ls args))
        (if (null? ls)
          #t
          (let ((datum (car ls)))
            (if (eq? 'weak-nl datum)
              (or after-weak-nl? (display #\newline port))
              (display datum port))
            (set! after-weak-nl? (eq? 'weak-nl datum))
            (loop (cdr ls))))))))

(define (texml-wr-has-option?2 opt rest)
  (let ((kids (car rest)))
    (if (or (null? kids) (null? (cdr kids)))
      #t
      (memq opt (caddr kids)))))
(define (texml-wr-has-option? opt kids)
  (and (not (null? kids)) (memq opt (car kids))))

(define (texml-serialize doc cout)
  (letrec ((st `(
      (texml . ,sxslt-id)
      (cmd *preorder* . ,(lambda (cmd-tag cmd-name . rest)
                           (cout #\\ cmd-name)
                           (if (null? rest)
                             (cout "{}")
                             (pre-post-order rest st))
                           (if (texml-wr-has-option? 'nonl2 rest)
                             #f
                             (cout 'weak-nl))))
      (gr *preorder* . ,(lambda (gr-tag . rest)
                          (cout #\{)
                          (pre-post-order rest st)
                          (cout #\})))
      (env *preorder* . ,(lambda (env-tag env-name . rest)
                          (cout 'weak-nl "\\begin{" env-name #\})
                          (if (texml-wr-has-option? 'nonl2 rest)
                             #f
                             (cout 'weak-nl))
                          (pre-post-order rest st)
                          (if (texml-wr-has-option? 'nonl3 rest)
                            #f
                            (cout 'weak-nl))
                          (cout "\\end{" env-name #\} 'weak-nl)))
      (*text* . ,(lambda (dummy s) (cout s)))
      (wr *preorder* . ,(lambda dummy #f))
      (*default* *preorder* . ,(lambda args
                 (cerr "*** Unknown TeXML element: " (car args) #\newline)
                 (cout "\\ERROR{" args "}")))
      )))
    (pre-post-order doc st)))

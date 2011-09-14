; converting tables

; some generic parameters are required
(define (convert-table tgroup-node conv-map)
  (pre-post-order tgroup-node `(
      (,(db "tgroup") . ,(lambda (_ . kids) `(env calstable ,@kids)))
      (,(db "thead")  . ,(lambda (_ . kids) `(env thead ,@kids)))
      (,(db "tbody")  . ,(lambda (_ . kids) kids))
      (,(db "row")    . ,(lambda (_ . kids) `((cmd brow (wr nogr)) ,@kids (cmd erow (wr nogr)))))
      (,(db "entry")  . ,(lambda (_ . kids) `(cmd cell (gr ,@kids))))
      ,@conv-map)))

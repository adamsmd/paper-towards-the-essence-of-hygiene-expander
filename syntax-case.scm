;(define-typed-subrecord k-syntax k-syntax-case () (scr k-syntax?) (lits null?) (clauses (clauses? pattern? k-syntax?)))


;; Patterns

#|
(define-record-type pattern)
(define-typed-subrecord pattern p-wild ())
(define-typed-subrecord pattern p-nil ())
(define-typed-subrecord pattern p-var () (ident ref-atom?))
(define-typed-subrecord pattern p-pair () (fst pattern?) (snd pattern?))

#;(define (map-pattern-ident f pattern)
  (match pattern ()
   [#(ident r b) (f pattern)] ;; (why do we need #(...) instead of #[...] ???)
   [() pattern]
   [(p1 . p2)
    (let ([p1^ (map-pattern-ident p1)]
          [p2^ (map-pattern-ident p2)])
      (make-p-pair p1^ p2^))]))
|#


#;(define (expand-clause clause)
  (match clause
   [(pattern body)
    (let-values ([(b r p) (freshen-pattern pattern)])
      (list p (subst* b r body)))]))

#;(define (expand-pattern u-syntax)
  (match u-syntax (_)
   [#(ident #(ref-atom _) b0) (values '() (make-p-wild))]
   [#(ident r b)
    (let ([r^ (gensym-ref-atom)])
      (values (list b) (list r^) (make-p-var r^)))]
   [() (values '() (make-p-nil))]
   [(p1 . p2)
    (let-values ([(b1 r1 p1^) (expand-pattern p1)]
                 [(b2 r2 p2^) (expand-pattern p2)])
      (values (append b1 b2) (append r1 r2)
              (make-p-pair p1^ p2^)))]))



(library (util record-match)
  (export match define-match)
  (import (rnrs) (util record-match-helpers))

;; This library implements a 'match' pattern matching form that makes
;; it easy to pattern match records and a 'define-match' form that
;; makes it easy to define functions that pattern match their
;; argument.
;;
;; Note that this implementation is not efficient.  In addition, due
;; to phase restrictions in Scheme, the main functions implementing
;; matching are in the record-match-helpers library.

;; Usage: (match scr (lits ...) [pat guard body] ...)
;;  - Scr is an expression that is pattern matched
;;  - Lits is a lit of literals that are treated as literal *symbols* in the following patterns
;;  - Pat is a pattern.  Notably we use vector notation to pattern match records (e.g., #(rec-name field1 field2)).
;;  - Guard is a predicate that is tested to see whether to run the body.  This part is optional and treated as #f if omitted.
;;  - Body is an expression to run if the pattern matches and the guard passes.

(define-syntax match
  (lambda (stx)
    (syntax-case stx ()
      [(_ scr lits . clauses)
       (let ()
         (define (mk-clauses clauses)
           (syntax-case clauses ()
             [() #'(error 'match "unmatched value" tmp)]
             [([pat body] . rest) (mk-clauses #'([pat #t body] . rest))]
             [([pat guard body] . rest)
              #`(let ([next-clause (lambda () #,(mk-clauses #'rest))]
                      [env (match-pattern (syntax->list #'lits) #'#,(escape-ellipses #'pat) tmp)])
                  (if env
                      (let #,(map (lambda (key) #`[#,key (cdr (assp (curry bound-identifier=? #'#,key) env))])
                                  (pattern-vars (syntax->list #'lits) #'pat))
                        (if guard
                            body
                            (next-clause)))
                      (next-clause)))]))
         #`(let ([tmp scr]) #,(mk-clauses #'clauses)))])))

;; This form defines a function that pattern matches its argument.
;;
;; Usage #1: (define-match func [pat body] ...)
;;   Defines a function 'func' that takes one argument that is pattern matched against each 'pat'.
;;
;; Usage #2: (define-match (func pat) body)
;;   This is a simplified version of define-match for when there is only one pattern match clause
(define-syntax define-match
  (syntax-rules ()
    [(_ (name pattern) body ...) (define-match name [pattern (begin body ...)])]
    [(_ name clauses ...) (define (name tmp)
                                   (match tmp ()
                                     clauses ...
                                     [_ (error 'name "unmatched input" tmp)]))]))
  

#;(define (tests)
(print-gensym 'pretty)
(expand '(match x () [a b]))
(expand '(match x () [a b c]))
(expand '(match x () [a b c] [i j k]))
(expand '(match x () [(1 . 2) b]))
(expand '(match x () [() y]))
(expand '(match x () [(l y) y]))
(expand '(match x (l m) [(l y) y]))
(expand '(match x (l m) [(l y) y][(m z) z]))
(define-record-type pair (fields fst snd))
(expand '(match x () [#(pair y z) 3]))
(match (make-pair 1 2) () [#(pair x y) (list x y)])
)

)

(library (record-match)
  (export match)
  (import (rnrs))

;; This library implements a 'match' pattern matching form that makes
;; it easy to pattern match records.
;;
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
         (define count 0)
         (define (gensym)
           (set! count (+ 1 count))
           (string->symbol (string-append "tmp" (number->string count))))
         (define (is-lit? l)
           (define (go lits)
             (syntax-case lits ()
               [() #f]
               [(lit . lits) (or (free-identifier=? l #'lit) (go #'lits))]))
           (go #'lits))
         (define (mk-fields scr name number fields succ)
           (syntax-case fields ()
             [() succ]
             [(field . fields)
              (mk-pred #`((record-accessor (record-type-descriptor #,name) #,number) #,scr) #'field
                       (mk-fields scr name (+ number 1) #'fields succ))]))
         (define (mk-pred scr pat succ)
           ;; identifier to examine * pattern * syntax on success -> syntax to implement the pattern match
           ;; assumes the identifier 'fail is bound to a failure continuation
           (with-syntax ([tmp (datum->syntax #'tmp (gensym))])
           #`(let ([tmp #,scr])
               #,(syntax-case pat ()
                   [id (identifier? #'id)
                    (cond
                     [(free-identifier=? #'id #'_) succ]
                     [(is-lit? #'id) #`(if (equal? tmp 'id) #,succ (fail))]
                     [else #`(let ([id tmp]) #,succ)])]
                   [(pat-car . pat-cdr)
                    #`(if (pair? tmp)
                          #,(mk-pred #'(car tmp) #'pat-car
                                     (mk-pred #'(cdr tmp) #'pat-cdr
                                              succ))
                          (fail))]
                   [#(name fields ...)
                    #`(if (and (record? tmp)
                               (equal? (record-rtd tmp) (record-type-descriptor name))
                               (equal? (vector-length (record-type-field-names (record-type-descriptor name)))
                                       #,(length (syntax->datum #'(fields ...)))))
                          #,(mk-fields scr #'name 0 #'(fields ...) succ)
                          (fail))]
                   [atom (let ([x (syntax->datum #'atom)])
                           (or (boolean? x) (number? x) (char? x)
                               (string? x) (null? x)))
                    #`(if (equal? tmp 'atom) #,succ (fail))]
                   [_ (syntax-violation 'match "unknown pattern form" pat)]))))
         (define (mk-clauses clauses)
           (syntax-case clauses ()
             [() #'(error 'match "unmatched value" tmp)]
             [([pat body] . rest) (mk-clauses #'([pat #t body] . rest))]
             [([pat guard body] . rest)
              #`(let ([fail (lambda () #,(mk-clauses #'rest))])
                  #,(mk-pred #'tmp #'pat #'(if guard body (fail))))]))
         #`(let ([tmp scr]) #,(mk-clauses #'clauses)))])))

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

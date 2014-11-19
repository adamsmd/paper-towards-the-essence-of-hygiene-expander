(library (u-syntax)
  (export constant? u-syntax? u-syntax-map-idents)
  (import (rnrs) (typed-records) (idents))

;; This library defines types and operations for u-syntax, which
;; represents code that is not yet fully expanded and thus has an
;; unknown binding structure.

;; The constant forms allowed in a u-syntax.  Note symbols are not
;; allowed.  Identifiers should be used instead.
(define (constant? x)
  (or (boolean? x) (number? x) (char? x) (string? x) (null? x)))

;; A predicate defining a u-syntax as an ident, constant, or pair of
;; u-syntax.
(define (u-syntax? x)
  (or (ident? x)
      (constant? x)
      (and (pair? x) (u-syntax? (car x)) (u-syntax? (cdr x)))))

;; A function that applies 'f' to all ident in a u-syntax.
(define (u-syntax-map-idents f s)
  (cond
   [(ident? s) (f s)]
   [(constant? s) s]
   [(pair? s) (cons (u-syntax-map-idents f (car s))
                    (u-syntax-map-idents f (cdr s)))]))

)

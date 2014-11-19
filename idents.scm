(library (idents)
  (export
   ident ident? make-ident ident-ref ident-bind
   ident-ref=? ident-bind=?
   prettify-ident
   )
  (import (rnrs) (typed-records) (atoms))

;; This library defines types and operations for identifiers

;; An identifier is a diatomic pair of a ref-atom and a bind-atom.
(define-typed-record ident () (ref ref-atom?) (bind bind-atom?))

;; ident-ref=? and ident-bind=? respectively test whether the
;; ref-atoms or bind-atoms in two identifiers are equal.
(define (ident-ref=? i1 i2) (ref-atom-equal? (ident-ref i1) (ident-ref i2)))
(define (ident-bind=? i1 i2) (bind-atom-equal? (ident-bind i1) (ident-bind i2)))

;; Convert an ident to a vector notation that pretty prints better.
(define (prettify-ident s)
  (vector (ref-atom-name (ident-ref s))
          (bind-atom-name (ident-bind s))))

)

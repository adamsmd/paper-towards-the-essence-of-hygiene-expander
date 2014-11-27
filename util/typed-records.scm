(library (util typed-records)
  (export define-typed-record define-algebraic-type list-of?)
  (import (rnrs))

;; This library defines forms for defining record types that check the
;; type of the objects they contain.

;; Defines a record type that checks the type of the objects it contains.
;; Usage: (define-typed-record name (field pred) ...)
;;  - Name is the name of the record type
;;  - Field and pred are the field names and the predicate that that field must satisfy.
(define-syntax define-typed-record
  (syntax-rules ()
    [(_ self (field pred) ...)
     (define-record-type self
       (nongenerative self)
       (fields field ...)
       (protocol
        (lambda (make)
          (lambda (field ...)
            (if (not (pred field)) (assertion-violation 'self "invalid argument type" 'pred 'field field)) ...
            (make field ...)))))]))

;; TODO
(define-syntax define-algebraic-type
  (syntax-rules ()
    [(_ type-name [ctor (field pred) ...] ...)
     (begin
       (define-record-type type-name)
       (define-record-type ctor
         (parent type-name)
         (nongenerative ctor)
         (fields field ...)
         (protocol
          (lambda (make)
            (lambda (field ...)
              (if (not (pred field)) (assertion-violation 'self "invalid argument type" 'pred 'field field)) ...
              ((make) field ...))))) ...)]))
        

;; A helper function for defining list types
;; Usage: ((list-of? f) x)
;;  Returns true iff x is a list and f returns true on all elements in that list.
(define (list-of? f) (lambda (x) (and (list? x) (for-all f x))))

;; TODO: non-empty-list-of?
;; TODO: non-duplicating-list-of?

)

(library (types)
  (export
   ;; Atoms
   ref-atom ref-atom? make-ref-atom ref-atom-name
   bind-atom bind-atom? make-bind-atom bind-atom-name
   ref-atom-equal? bind-atom-equal? 
   gensym-ref-atom gensym-bind-atom

   ;; Identifiers
   ident ident? make-ident ident-ref ident-bind
   ident-ref=? ident-bind=?
   prettify-ident

   ;; U-syntax
   constant? u-syntax? u-syntax-map-idents

   ;; K-syntax
   k-syntax k-syntax?
   k-const make-k-const k-const?
   k-var make-k-var k-var?
   k-lam make-k-lam k-lam?
   k-app make-k-app k-app?
   k-if make-k-if k-if?
   k-let make-k-let k-let?
   k-letrec make-k-letrec k-letrec?
   k-let-syntax make-k-let-syntax k-let-syntax?
   k-letrec-syntax make-k-letrec-syntax k-letrec-syntax?
   k-syn make-k-syn k-syn?
   k-u-syntax make-k-u-syntax k-u-syntax?

   map-bindings
   )

  (import (rnrs) (util gensym) (util typed-records))

;; TODO

;;;;;;;;;;;;;;;;
;; Atoms

;; This library defines types and operations for ref-atoms and bind-atoms.

;; There are two types of atoms: ref-atom and bind-atom.  Each unique
;; atom is represented by a symbolic name.
(define-typed-record ref-atom (name symbol?))
(define-typed-record bind-atom (name symbol?))

;; ref-atom-equal? and bind-atom-equal? respectively test whether two
;; ref-atoms or bind-atoms have the name symbolic name.
(define (ref-atom-equal? r1 r2) (equal? (ref-atom-name r1) (ref-atom-name r2)))
(define (bind-atom-equal? b1 b2) (equal? (bind-atom-name b1) (bind-atom-name b2)))

;; gensym-ref-atom and gensym-bind-atom generate unique, fresh
;; versions of ref-atoms or bind-atoms, respectively
(define (gensym-ref-atom a) (make-ref-atom (gensym (ref-atom-name a))))
(define (gensym-bind-atom a) (make-bind-atom (gensym (bind-atom-name a))))

;;;;;;;;;;;;;;;;
;; Identifiers

;; This library defines types and operations for identifiers

;; An identifier is a diatomic pair of a ref-atom and a bind-atom.
(define-typed-record ident (ref ref-atom?) (bind bind-atom?))

;; ident-ref=? and ident-bind=? respectively test whether the
;; ref-atoms or bind-atoms in two identifiers are equal.
(define (ident-ref=? i1 i2) (ref-atom-equal? (ident-ref i1) (ident-ref i2)))
(define (ident-bind=? i1 i2) (bind-atom-equal? (ident-bind i1) (ident-bind i2)))

;; Convert an ident to a vector notation that pretty prints better.
(define (prettify-ident s)
  (vector (ref-atom-name (ident-ref s))
          (bind-atom-name (ident-bind s))))

;;;;;;;;;;;;;;;;
;; U-syntax

;; This library defines types and operations for u-syntax, which
;; represents code that is not yet fully expanded and thus has an
;; unknown binding structure.  Since u-syntax is just an s-expr with
;; identifiers instead of atoms, we don't define any new types but do
;; define some predicates and a mapping function.

;; A predicate defining a u-syntax as an ident, constant, or pair of
;; u-syntax objects.
(define (u-syntax? x)
  (or (ident? x)
      (constant? x)
      (and (pair? x) (u-syntax? (car x)) (u-syntax? (cdr x)))))

;; The constant forms allowed in a u-syntax.  Note symbols are not
;; allowed.  Identifiers should be used instead.
(define (constant? x)
  (or (boolean? x) (number? x) (char? x) (string? x) (null? x)))

;; A function that applies 'f' to all ident in a u-syntax.
(define (u-syntax-map-idents f s)
  (cond
   [(ident? s) (f s)]
   [(constant? s) s]
   [(pair? s) (cons (u-syntax-map-idents f (car s))
                    (u-syntax-map-idents f (cdr s)))]))

;; This library defines types and operations for k-syntax, which
;; represents code that is already fully expanded and thus has a known
;; binding structure.

;;;;;;;;;;;;;;;;
;; K-syntax

(define-algebraic-type k-syntax
  [k-const (value constant?)]
  [k-var (ident ref-atom?)]
  [k-lam (args (list-of? ref-atom?)) (body (list-of? k-syntax?))]
  [k-app (fun k-syntax?) (args (list-of? k-syntax?))]
  [k-if (test k-syntax?) (true k-syntax?) (false k-syntax?)]
  [k-let (bindings bindings?) (body (list-of? k-syntax?))]
  [k-letrec (bindings bindings?) (body (list-of? k-syntax?))]
  [k-let-syntax (bindings bindings?) (body (list-of? k-syntax?))]
  [k-letrec-syntax (bindings bindings?) (body (list-of? k-syntax?))]
  [k-syn (value u-syntax?)]
  [k-u-syntax (value u-syntax?)])

;;;;
;; Binding forms for a let, letrec, let-syntax or letrec-syntax

(define (binding? x) (and (list? x) (eq? 2 (length x)) (ref-atom? (car x)) (k-syntax? (cadr x))))
(define bindings? (list-of? binding?))
(define (map-bindings f bindings) (map (lambda (binding) (list (car binding) (f (cadr binding)))) bindings))

)

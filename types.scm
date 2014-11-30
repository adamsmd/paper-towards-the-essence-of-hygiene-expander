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
   k-syntax-quote make-k-syntax-quote k-syntax-quote?
   k-u-syntax make-k-u-syntax k-u-syntax?

   map-bindings
   )

  (import (rnrs) (util gensym) (util typed-records))

;; This library defines records and functions for the basic types used
;; in the expander.
;;
;; The basic intuition behind these types are as follows.
;;
;; == Atoms ==
;;
;; There are two types or sorts of atoms.  These are reference atoms
;; (represented by the ref-atom type) and binder atoms (represented by
;; the bind-atom type).  The places where these may occur are mutually
;; disjoint.
;;
;; A ref-atom is used to keep track the binding forms that identifiers
;; may refer to.  They are the only sort of atom used in the fully
;; expanded, outer k-syntx.  They also occur in the reference parts of
;; identifiers.
;;
;; A bind-atom is used to keep track of which identifiers could
;; potentially bind which other identifiers in the not-fully expanded,
;; inner u-syntax.  They occur only in the binder parts of
;; identifiers.
;;
;; The expansion process generates fresh versions of both sorts of
;; atoms, but the initial versions of these atoms are not freshly
;; generated.  Also there is nothing in the symbolic names of these
;; atoms to distinguish these two sorts of atoms.  Thus when pretty
;; printing expansion results, you must rely on where the atom occurs
;; to know which type it is.
;;
;; == Identifiers ==
;;
;; An ident is a diatomic identifier made up of a pair of a ref-atom
;; (called its reference part) and a bind-atom (called its binder
;; part).  When pretty printing to the user, we represent identifiers
;; using Scheme's vector notation.  Thus the following ident
;; would be printed simply as #(r b):
;;
;;   #[ident #[ref-atom r] #[bind-atom b]]
;;
;; When 'r' and 'b' are identical, we simplify this further and when
;; prettying printing, use use the symbol 'r'.
;;
;; == U-syntax ==
;;
;; U-syntax is not yet fully expanded and thus has an *unknown*
;; binding structure.  Since it is not yet fully expanded into core
;; forms, u-syntax is basically just an s-expression with identifiers
;; instead of symbols.
;;
;; == K-syntax ==
;;
;; K-syntax is expanded into core forms and thus has a *known* binding
;; structure.  Since what core forms are being used is known, we
;; represent it using records.
;;
;; Within a syntax tree, k-syntax will occur in the outer parts while
;; u-syntax occurs in inner parts.  A k-syntax never occurs inside a
;; u-syntax.

;; ===========
;; == Atoms ==
;; ===========

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

;; =================
;; == Identifiers ==
;; =================

;; An identifier is a diatomic pair of a ref-atom and a bind-atom.
(define-typed-record ident (ref ref-atom?) (bind bind-atom?))

;; ident-ref=? and ident-bind=? respectively test whether the
;; ref-atoms or bind-atoms in two identifiers are equal.
(define (ident-ref=? i1 i2) (ref-atom-equal? (ident-ref i1) (ident-ref i2)))
(define (ident-bind=? i1 i2) (bind-atom-equal? (ident-bind i1) (ident-bind i2)))

;; Convert an ident to a vector notation that pretty prints better.
(define (prettify-ident s)
  (let ([r (ref-atom-name (ident-ref s))]
        [b (bind-atom-name (ident-bind s))])
    (if (eq? r b) r (vector r b))))

;; ==============
;; == U-syntax ==
;; ==============

;; A u-syntax represents code that is not yet fully expanded and thus
;; has an unknown binding structure.  Since u-syntax is just an s-expr
;; with identifiers instead of atoms, we don't define any new types
;; but do define some predicates and a mapping function.

;; 'u-syntax?' is a predicate defining a u-syntax as an ident,
;; constant or pair of u-syntax objects.
(define (u-syntax? x)
  (or (ident? x)
      (constant? x)
      (and (pair? x) (u-syntax? (car x)) (u-syntax? (cdr x)))))

;; The constant forms allowed in a u-syntax.  Note that symbols are
;; not allowed.  Identifiers should be used instead.
(define (constant? x)
  (or (boolean? x) (number? x) (char? x) (string? x) (null? x)))

;; A function that applies 'f' to all ident in a u-syntax.
(define (u-syntax-map-idents f s)
  (cond
   [(ident? s) (f s)]
   [(constant? s) s]
   [(pair? s) (cons (u-syntax-map-idents f (car s))
                    (u-syntax-map-idents f (cdr s)))]))

;; ==============
;; == K-syntax ==
;; ==============

;; K-syntax represents code that is already fully expanded and thus
;; has a known binding structure.  The only unusual form is k-u-syntax
;; which contains u-syntax waiting to be expanded.

(define-algebraic-type k-syntax
  [k-u-syntax (value u-syntax?)] ;; u-syntax that is not yet fully expanded
  [k-const (value constant?)] ;; quoted constants
  [k-var (ident ref-atom?)] ;; variable references
  [k-lam (args (list-of? ref-atom?)) (body (list-of? k-syntax?))] ;; lambdas
  [k-app (fun k-syntax?) (args (list-of? k-syntax?))] ;; function applications
  [k-if (test k-syntax?) (true k-syntax?) (false k-syntax?)] ;; 'if' conditionals
  [k-let (bindings bindings?) (body (list-of? k-syntax?))] ;; 'let' binding
  [k-letrec (bindings bindings?) (body (list-of? k-syntax?))] ;; 'letrec' binding
  [k-let-syntax (bindings bindings?) (body (list-of? k-syntax?))] ;; 'let-syntax' binding
  [k-letrec-syntax (bindings bindings?) (body (list-of? k-syntax?))] ;; 'letrec-syntax' binding
  [k-syntax-quote (value u-syntax?)] ;; 'syntax' quotation form
  )

;; Several k-syntax forms have binding clauses so we also define
;; predicate and mapping functions for those.  Bindings are all of the
;; form:
;;
;;   ((ref-atom k-syntax) ...)
(define bindings?
  (list-of? (lambda (x)
              (and (list? x) (eq? 2 (length x))
                   (ref-atom? (car x))
                   (k-syntax? (cadr x))))))

;; map-bindings applies 'f' to the k-syntax in the rhs of 'bindings'
(define (map-bindings f bindings)
  (map (lambda (binding) (list (car binding) (f (cadr binding)))) bindings))

)

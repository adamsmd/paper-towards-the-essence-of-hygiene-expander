(library (s-exprs)
  (export
   s-expr->u-syntax s-expr->k-syntax k-syntax->s-expr
   s-expr-map-type s-expr-prettify-idents s-expr-prettify-gensyms)
  (import (rnrs) (record-match) (gensym)
          (atoms) (idents) (k-syntax))

;; This library implements injection and projections for s-exprs to
;; and from k-syntax and u-syntax as well as functions for mapping
;; across s-exprs.

;;;;;;;;;;;;;;;;
;; From s-expr

;; Converts from s-expr to u-syntax by converting symbols to ident objects
(define (s-expr->u-syntax s-expr)
  (s-expr-map-type symbol? (lambda (s) (make-ident (make-ref-atom s) (make-bind-atom s))) s-expr))

;; Converts from s-expr to k-syntax by wrapping a k-u-syntax around the u-syntax for the s-expr
(define (s-expr->k-syntax s-expr) (make-k-u-syntax (s-expr->u-syntax s-expr)))

;;;;;;;;;;;;;;;;
;; To s-expr

;; Converts a k-syntax to an s-expr.  Leaves the contents of any
;; contained u-syntax unchanged.
(define (k-syntax->s-expr k-syntax)
  (define (rec k-syntax)
    (match k-syntax ()

     ;; Boilerplate recursions
     [#(k-u-syntax value) `(u-syntax ,value)]
     [#(k-syn syn) `(quote ,syn)]
     [#(k-const c) `(quote ,c)]
     [#(k-var #(ref-atom r)) r]
     [#(k-lam args body) `(lambda ,(map ref-atom-name args) . ,(map rec body))]
     [#(k-app fun args) `(,(rec fun) . ,(map rec args))]
     [#(k-if test true false) `(if ,(rec test) ,(rec true) ,(rec false))]

     ;; Recursions with bindings
     [#(k-let bindings body)
      `(let ,(map (lambda (b) `(,(ref-atom-name (car b)) ,(rec (cadr b)))) bindings) . ,(map rec body))]
     [#(k-letrec bindings body)
      `(letrec ,(map (lambda (b) `(,(ref-atom-name (car b)) ,(rec (cadr b)))) bindings) . ,(map rec body))]
     [#(k-let-syntax bindings body)
      `(let-syntax ,(map (lambda (b) `(,(ref-atom-name (car b)) ,(rec (cadr b)))) bindings) . ,(map rec body))]
     [#(k-letrec-syntax bindings body)
      `(letrec-syntax ,(map (lambda (b) `(,(ref-atom-name (car b)) ,(rec (cadr b)))) bindings) . ,(map rec body))]
     ))

  (rec k-syntax))

;;;;;;;;;;;;;;;;
;; Mapping across s-exprs

;; Applies 'f' to any objects satisfying 'type?' in the s-expr 's'.
(define (s-expr-map-type type? f s)
  (define (rec s) (s-expr-map-type type? f s))
  (cond
   [(type? s) (f s)]
   [(pair? s) (cons (rec (car s)) (rec (cdr s)))]
   [(vector? s) (vector-map rec s)]
   [else s]))

;; Applies prettify-ident to all ident in an s-expr
(define (s-expr-prettify-idents s) (s-expr-map-type ident? prettify-ident s))

;; Applies prettify-gensym to all gensyms in an s-expr
(define (s-expr-prettify-gensyms s) (s-expr-map-type gensym? prettify-gensym s))

)

(library (s-exprs)
  (export
   s-expr->u-syntax s-expr->k-syntax k-syntax->s-expr
   s-expr-map-type s-expr-prettify-idents)
  (import (rnrs) (util record-match) (types))

;; This library implements injection and projections for s-exprs to
;; and from k-syntax and u-syntax as well as functions for mapping
;; across s-exprs.  This is so that we can use a "pretty" notation
;; when reading from and writing out to the user.  Nothing interesting
;; with respect to hygiene occurs in this file.
;;
;; The s-expression notation we use is standard except that for
;; diatomic identifiers we cannot use symbols.  Since R6RS record
;; notation is verbose and difficult to read, we steal vector notation
;; and use #(r b) to represent #[ident #[ref-atom r] #[bind-atom b]]
;; when r and b are not equal.  When they are equal, we just use the
;; symbol r.

;; =================
;; == From s-expr ==
;; =================

;; Converts from s-expr to u-syntax by converting symbols to ident objects
(define (s-expr->u-syntax s-expr)
  (s-expr-map-type
   symbol? (lambda (s) (make-ident (make-ref-atom s) (make-bind-atom s)))
   s-expr))

;; Converts from s-expr to k-syntax by wrapping a k-u-syntax around
;; the u-syntax for the s-expr
(define (s-expr->k-syntax s-expr) (make-k-u-syntax (s-expr->u-syntax s-expr)))

;; ===============
;; == To s-expr ==
;; ===============

;; Converts a k-syntax to an s-expr.  Leaves the contents of any
;; contained u-syntax unchanged.  If 'for-eval'? is true, then we
;; slightly modify the output so it is appropriate for passing to
;; 'eval'.  Specifically, we remove let-syntax and letrec-syntax, and
;; we use 'quote' instead of 'syntax' as we do not want to assume the
;; underlying system implements these forms.  (After all, that is the
;; job of the expander we are implementing.)  Fortunately, since the
;; k-syntax in make-macro-transformer is already fully expanded, we
;; can just replace any let-syntax or letrec-syntax with a 'let' with
;; no binders as any macro calls to the macros bound by them are
;; already expanded.

(define k-syntax->s-expr
  (case-lambda
   [(k-syntax) (k-syntax->s-expr #f k-syntax)]
   [(for-eval? k-syntax)
    (define (rec k-syntax)
      (match k-syntax ()

       ;; Boilerplate recursions
       [#(k-u-syntax value) `(u-syntax ,value)]
       [#(k-syntax-quote syn) `(,(if for-eval? 'quote 'syntax) ,syn)]
       [#(k-const c) `(quote ,c)]
       [#(k-var #(ref-atom r)) r]
       [#(k-lam args body) `(lambda ,(map ref-atom-name args) . ,(map rec body))]
       [#(k-app fun args) `(,(rec fun) . ,(map rec args))]
       [#(k-if test true false) `(if ,(rec test) ,(rec true) ,(rec false))]

       ;; Recursions with bindings
       [#(k-let bindings body)
        `(let ,(map (lambda (b) `(,(ref-atom-name (car b)) ,(rec (cadr b)))) bindings)
           . ,(map rec body))]
       [#(k-letrec bindings body)
        `(letrec ,(map (lambda (b) `(,(ref-atom-name (car b)) ,(rec (cadr b)))) bindings)
           . ,(map rec body))]
       [#(k-let-syntax bindings body)
        `(let-syntax ,(if for-eval? '() (map (lambda (b) `(,(ref-atom-name (car b)) ,(rec (cadr b)))) bindings))
           . ,(map rec body))]
       [#(k-letrec-syntax bindings body)
        `(letrec-syntax ,(if for-eval? '() (map (lambda (b) `(,(ref-atom-name (car b)) ,(rec (cadr b)))) bindings))
           . ,(map rec body))]
       ))

    (rec k-syntax)]))

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

)

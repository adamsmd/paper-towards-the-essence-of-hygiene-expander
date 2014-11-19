(library (expand)
  (export
   expand-s-expr expand-k-syntax* expand-k-syntax expand-u-syntax make-macro-transformer

   env0
   lambda-transformer
   if-transformer
   let-transformer
   letrec-transformer
   let-syntax-transformer
   letrec-syntax-transformer
   syntax-transformer

   hyg subst subst*
   )
  (import (rnrs) (rnrs eval)
          (record-match) (typed-records)
          (atoms) (idents) (u-syntax) (k-syntax) (s-exprs)
          )
  
;; This library implements the main expander

;;;;;;;;;;;;;;;;
;; Wrappers/drivers

;; This function acts as a wrapper for the expander that goes from
;; s-expr to s-expr.
;;
;; The 'n' parameter is the maximum number of expansion steps to take
;; or #f for no limit.
;;
;; You should usually pass env0 as the env parameter.

(define (expand-s-expr n env s-expr)
  (s-expr-prettify-idents
   (k-syntax->s-expr
    (expand-k-syntax* n env (s-expr->k-syntax s-expr)))))

;; This function acts as a wrapper for the expander that goes from
;; k-syntax to k-syntax.  It is called by expand-s-expr and its 'n'
;; and env parameters are the same as for expand-s-expr.

(define (expand-k-syntax* n env k-syntax)
  (if (and n (= n 0))
      k-syntax
      (let ([k-syntax^ (expand-k-syntax env k-syntax)])
        (if (not k-syntax^)
            k-syntax
            (expand-k-syntax* (and n (- n 1)) env k-syntax^)))))

;;;;;;;;;;;;;;;;
;; Expanding K-Syntax

;; This function does one step of expansion for k-syntax.
;; If there is not expansion to be done, it returns #f.
;;
;; This function is basically just a traversal to find a u-syntax in
;; the k-syntax to expand.  However, it does extend the environment
;; whenever it finds a binding form.  During this process
;; make-macro-transformer is used to create the function that
;; implements macro transformers.

(define (expand-k-syntax env k-syntax)
  (define (extend-env form on-rhs lhs rhs env)
    (case form
      ['let (if on-rhs env (cons (cons lhs #f) env))]
      ['letrec (cons (cons lhs #f) env)]
      ['let-syntax (if on-rhs env (cons (cons lhs (make-macro-transformer rhs)) env))]
      ;; Note that the spec doesn't say what happens when expanding
      ;; the rhs of a letrec-syntax requires calling a macro also
      ;; defined in that letrec-syntax.  We disallow this.
      ['letrec-syntax (cons (cons lhs (if on-rhs #f (make-macro-transformer rhs))) env)]))
  ((traverse-k-syntax expand-u-syntax extend-env) env k-syntax))

;; This function creates a function implementing the macro transformer
;; for a particular bit of k-syntax.
;;
;; In order to implement this, we project the k-syntax to an s-expr and use Scheme's 'eval'.
;;
;; We also use 'hyg' to ensure that the resulting function is hygienic.

(define (make-macro-transformer k-syntax)
  (define f (eval (k-syntax->s-expr (remove-let-syntax k-syntax))
                  (environment '(except (rnrs) free-identifier=? bound-identifier=?) '(expand-primitives))))
  (lambda (macro-call) (make-k-u-syntax ((hyg f) macro-call))))

;; This function removes let-syntax and letrec-syntax from a k-syntax.
;; This is used in make-macro-transformer when projecting a k-syntax
;; to an s-expr as we do not want to assume the underlying system
;; implements let-syntax or letrec-syntax.  (After all, that is the
;; job of the expander implemented in this file.)  Fortunately, since
;; the k-syntax in make-macro-transformer is already fully expanded,
;; we can just replace any let-syntax or letrec-syntax with a 'let'
;; with no binders.

(define remove-let-syntax
  (map-k-syntax (lambda (k-syntax)
                  (match k-syntax ()
                   [#(k-let-syntax _ body) (make-k-let '() body)]
                   [#(k-letrec-syntax _ body) (make-k-let '() body)]
                   [else k-syntax]))))

;;;;;;;;;;;;;;;;
;; Expanding U-Syntax

;; This function implements one step of expansion for u-syntax, which
;; is the main job of the expander.  Expansion of constants,
;; variables, and function applications is done directly, but forms
;; that look like macro calls are all dispatched to the environment.
;; These forms include not just macro calls but also things like
;; lambda, if, let, letrec, and syntax.  Structuring the code this way
;; means that locally defined macros can shadow these forms.

(define (expand-u-syntax env u-syntax)
  (match u-syntax (lambda if let letrec let-syntax letrec-syntax syntax syntax-case)
   [c (constant? c) (make-k-const c)]
   [#(ident r b)
    (let ([m (assq r env)])
      (cond
       [(and m (cdr m)) (error 'expand-u-syntax "out of phase or context" r m)]
       [else (make-k-var r)]))]
   [(#(ident #(ref-atom r) b) . args)
    (assq r env)
    (let ([m (assq r env)])
      (cond
       [(not m) (error 'expand-u-syntax "unbound variable" r)]
       [(not (cdr m)) (error 'expand-u-syntax "out of phase or context" r m)]
       [else ((cdr m) u-syntax)]))]
   [(fun . args) (make-k-app (make-k-u-syntax fun) (map make-k-u-syntax args))]))

;;;;;;;;;;;;;;;;
;; Helpers for core-form transformers

;; This macro defines a short-hand for defining the functions that
;; implement the core forms.
(define-syntax define-transformer
  (syntax-rules ()
    [(_ (name pat) body)
     (define (name tmp)
       (match tmp ()
        [pat body]
        [else (error 'name "invalid syntax" tmp)]))]))

;; This macro defines a short-hand for defining transformers
;; for let-style core forms.
(define-syntax define-let-style-transformer
  (syntax-rules ()
    [(_ name make #f) (define-let-style-transformer name make (lambda (b r^ rhs) rhs))]
    [(_ name make #t) (define-let-style-transformer name make subst*)]
    [(_ name make rhs-fun)
     (define-transformer (name (_ bindings . body))
       (let-values ([(r^ b) (freshen-idents (map car bindings))])
         (let ([rhs (map cadr bindings)])
           (make (map (lambda (lhs rhs) (list lhs (make-k-u-syntax (rhs-fun b r^ rhs)))) r^ rhs)
                 (map (lambda (body) (make-k-u-syntax (subst* b r^ body))) body)))))]))

;; This function is a helper for defining the core forms that splits a
;; list of 'ident' into a binder and reference part.  It then gensyms
;; the reference part.  The resulting bind-atoms and ref-atoms are
;; intended to be passed to 'subst*'.
(define (freshen-idents args)
  (values (map (lambda (arg) (gensym-ref-atom (ident-ref arg))) args)
          (map (lambda (arg) (ident-bind arg)) args)))

;;;;;;;;;;;;;;;;
;; Transformers for core forms

(define-transformer (lambda-transformer (_ args . bodyX))
  (let-values ([(r^ b) (freshen-idents args)])
    (make-k-lam r^ (map (lambda (body) (make-k-u-syntax (subst* b r^ body))) bodyX))))

(define-transformer (if-transformer (_ test true false))
  (make-k-if (make-k-u-syntax test) (make-k-u-syntax true) (make-k-u-syntax false)))

(define-transformer (syntax-transformer (_ body))
  (make-k-syn body))

;; Note that the syntax core forms have the same structure as their
;; non-syntax counter parts.  The difference shows up in what things
;; are bound to in the traversal in expand-k-syntax.
(define-let-style-transformer let-transformer make-k-let #f)
(define-let-style-transformer letrec-transformer make-k-letrec #t)
(define-let-style-transformer let-syntax-transformer make-k-let-syntax #f)
(define-let-style-transformer letrec-syntax-transformer make-k-letrec-syntax #t)

;; The default environment with bindings for each core form.
(define env0
  `((lambda . ,lambda-transformer)
    (if . ,if-transformer)
    (syntax . ,syntax-transformer)
    (let . ,let-transformer)
    (letrec . ,letrec-transformer)
    (let-syntax . ,let-syntax-transformer)
    (letrec-syntax . ,letrec-syntax-transformer)
    ))

;;;;;;;;;;;;;;;;
;; Enforcing hygiene

;; This function takes a function 'f' and returns a hygienic
;; version of it.

(define (hyg f)
  (lambda (arg)
    (define perm '())
    (define (make-perm ident)
      (let* ([b1 (ident-bind ident)]
             [b2 (gensym-bind-atom b1)])
        (set! perm (cons (cons (bind-atom-name b1) (bind-atom-name b2))
                         (cons (cons (bind-atom-name b2) (bind-atom-name b1)) perm)))))
    (define (apply-permutation perm u-syntax)
      (define (f ident)
        (let ([r (assq (bind-atom-name (ident-bind ident)) perm)])
          (if r
              (make-ident (ident-ref ident) (make-bind-atom (cdr r)))
              ident)))
      (u-syntax-map-idents f u-syntax))
    (u-syntax-map-idents make-perm arg) ;; generate the permutation
    (apply-permutation perm (f (apply-permutation perm arg))))) ;; takes advantage of the fact 'perm' is a self inverse


;;;;;;;;;;;;;;;;
;; Subst

;; This function traverses a u-syntax, body, and for any identifiers
;; with a binder part equal to b and replaces their reference part
;; with r.
(define (subst b r body)
  (define (f ident)
    (if (bind-atom-equal? (ident-bind ident) b)
        (make-ident r b)
        ident))
  (u-syntax-map-idents f body))

;; This function is the same as subst but it takes a list of binder
;; parts and reference parts;
(define (subst* bs rs body)
  (cond
   [(null? bs) body]
   [else (subst* (cdr bs) (cdr rs) (subst (car bs) (car rs) body))]))

)

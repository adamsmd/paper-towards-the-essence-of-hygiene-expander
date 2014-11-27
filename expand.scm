(library (expand)
  (export
;; these are all traversals.  nothing interesting with respect to hygiene happens here
   expand-s-expr expand-k-syntax* expand-k-syntax expand-u-syntax

;; transformers for core forms.  only thing interesting is that we
;; always generate fresh variable names and use subst* to rebind
;; the reference parts of any identifiers with the same binder part
;; as the bound identifiers
;; This part handles reference hygiene
   subst subst*
   env0
   lambda-transformer
   if-transformer
   let-transformer
   letrec-transformer
   let-syntax-transformer
   letrec-syntax-transformer
   syntax-transformer

;; the construction of a hygienic macro transformer.
;; Note that 'hyg' takes a non-hygenic macro transformer
;; and turns it into a hygenic one.
;; this part handles binder hygiene.
   make-macro-transformer hyg
   )
  (import (rnrs) (rnrs eval)
          (util record-match) (util typed-records)
          (types) (s-exprs)
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

;; TODO: document
(define (map-seq f xs)
  (cond
   [(null? xs) '()]
   [else (let ([x (f (car xs))]) (cons x (map-seq f (cdr xs))))]))

(define-syntax seq
  (syntax-rules ()
    [(_ e0 e* ...) (seq0 () e0 e* ...)]))
(define-syntax seq0
  (syntax-rules ()
    [(_ (v ...) e0 e* ...) (let ([tmp e0]) (seq0 (v ... tmp) e* ...))]
    [(_ (v ...)) (v ...)]))

(define (expand-k-syntax env0 k-syntax)
  (define done #f)
  (define (rec0 env k-syntax)
    (define (rec k-syntax) (rec0 env k-syntax))
    (define (rec-let make form bindings body) (rec-let0 env make form bindings body))
    (if done
     k-syntax
     (match k-syntax ()
      ;; Apply "u-syntax-fun" to the u-syntax
      [#(k-u-syntax value) (begin (set! done #t) (expand-u-syntax env value))]

      ;; Binding forms
      [#(k-let bindings body) (rec-let make-k-let 'let bindings body)]
      [#(k-letrec bindings body) (rec-let make-k-letrec 'letrec bindings body)]
      [#(k-let-syntax bindings body) (rec-let make-k-let-syntax 'let-syntax bindings body)]
      [#(k-letrec-syntax bindings body) (rec-let make-k-letrec-syntax 'letrec-syntax bindings body)]

      ;; Everything else is a standard recursion
      [#(k-syn _) k-syntax]
      [#(k-const _) k-syntax]
      [#(k-var _) k-syntax]
      [#(k-lam args body) (seq make-k-lam args (map-seq rec body))]
      [#(k-app fun args) (seq make-k-app (rec fun) (map-seq rec args))]
      [#(k-if test true false) (seq make-k-if (rec test) (rec true) (rec false))])))

  ;; TODO: document this helper function
  (define (rec-let0 env make form bindings body)
    (define (rec env) (lambda (k-syntax) (rec0 env k-syntax)))
    (define (extend-env f)
      (append (map (lambda (binding) (cons (ref-atom-name (car binding)) (f (cadr binding)))) bindings) env))
    (define (rhs-env)
      (case form
        ;; TODO: document the logic behind these clauses
        [(let let-syntax) env]
        ;; Note that the spec doesn't say what happens when expanding
        ;; the rhs of a letrec-syntax requires calling a macro also
        ;; defined in that letrec-syntax.  We disallow this.
        [(letrec letrec-syntax) (extend-env (lambda (x) #f))]))
    (define (body-env)
      (case form
        [(let letrec) (extend-env (lambda (x) #f))]
        [(let-syntax letrec-syntax) (extend-env make-macro-transformer)]))
    (seq make (map-bindings (rec (rhs-env)) bindings)
              (if done body (map-seq (rec (body-env)) body))))

;; TODO: document
  (let ([x (rec0 env0 k-syntax)])
    (if done x #f)))

;;;;;;;;;;;;;;;;
;; Expanding U-Syntax

;; This function implements one step of expansion for u-syntax, which
;; is the main job of the expander.  Expansion of constants,
;; variables, and function applications is done directly, but forms
;; that look like macro calls are all dispatched to the corresponding
;; transformer in the environment.  These forms include not just macro
;; calls but also things like lambda, if, let, letrec, and syntax.
;; Structuring the code this way means that locally defined macros can
;; shadow these forms.

(define (expand-u-syntax env u-syntax)
  (match u-syntax ()
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

;;;;;;;;;;;;;;;;
;; Transformers for core forms

(define-transformer (lambda-transformer (_ (#(ident r b) ...) . body*))
  (let ([r^ (map gensym-ref-atom r)])
    (make-k-lam r^ (map (lambda (body) (make-k-u-syntax (subst* b r^ body))) body*))))

(define-transformer (if-transformer (_ test true-expr false-expr))
  (make-k-if (make-k-u-syntax test) (make-k-u-syntax true-expr) (make-k-u-syntax false-expr)))

(define-transformer (syntax-transformer (_ body))
  (make-k-syn body))

;; This macro defines a short-hand for defining transformers
;; for let-style core forms.
;; TODO: more detailed explanation
(define-syntax define-let-style-transformer-helper
  (syntax-rules ()
    [(_ name make rhs-fun)
     (define-transformer (name (_ ([#(ident r b) rhs] (... ...)) . body))
       (let ([r^ (map gensym-ref-atom r)])
         (make (map (lambda (lhs rhs) (list lhs (make-k-u-syntax (rhs-fun b r^ rhs)))) r^ rhs)
           (map (lambda (body) (make-k-u-syntax (subst* b r^ body))) body))))]))


;; Note that the syntax core forms have the same structure as their
;; non-syntax counter parts.  The difference shows up in what things
;; are bound to in the traversal in expand-k-syntax.
(define-syntax define-let-style-transformer
  (syntax-rules () [(_ name make) (define-let-style-transformer-helper name make (lambda (b r^ rhs) rhs))]))

(define-let-style-transformer let-transformer make-k-let)
(define-let-style-transformer let-syntax-transformer make-k-let-syntax)

(define-syntax define-letrec-style-transformer
  (syntax-rules () [(_ name make) (define-let-style-transformer-helper name make subst*)]))

(define-letrec-style-transformer letrec-transformer make-k-letrec)
(define-letrec-style-transformer letrec-syntax-transformer make-k-letrec-syntax)

;; TODO: explain how environments are defined

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
;; Macros

;; This function creates a function implementing the macro transformer
;; for a particular bit of k-syntax.
;;
;; In order to implement this, we project the k-syntax to an s-expr and use Scheme's 'eval'.
;;
;; We also use 'hyg' to ensure that the resulting function is hygienic.

(define (make-macro-transformer k-syntax)
;; TODO: note that f is not hyginic
;; TODO: macro-call is u-syntax
;; TODO: annotate with types
  (define f (eval (k-syntax->s-expr #t k-syntax) eval-environment))
  (lambda (macro-call) (make-k-u-syntax ((hyg f) macro-call))))

;; The environment to use when eval'ing the definition of a macro
;; TODO: note that must change if anything is added to expand-primitives (and comment in expand-primitives)
(define eval-environment
  (environment '(except (rnrs) free-identifier=? bound-identifier=?)
               '(expand-primitives)))


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
        (set! perm `((,(bind-atom-name b1) . ,(bind-atom-name b2))
                     (,(bind-atom-name b2) . ,(bind-atom-name b1))
                     . ,perm))))
    (define (apply-permutation perm u-syntax)
      (define (f ident)
        (let ([r (assq (bind-atom-name (ident-bind ident)) perm)])
          (if r
              (make-ident (ident-ref ident) (make-bind-atom (cdr r)))
              ident)))
      (u-syntax-map-idents f u-syntax))
    (u-syntax-map-idents make-perm arg) ;; generate the permutation
    (apply-permutation perm (f (apply-permutation perm arg))))) ;; takes advantage of the fact 'perm' is a self inverse

)

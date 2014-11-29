(library (expand)
  (export
   ;; Traversals that drive where expansion happens
   expand-s-expr expand-k-syntax* expand-k-syntax expand-u-syntax

   ;; Macro expansion
   make-macro-transformer hyg

   ;; Core-form expansion
   subst
   env0
   lambda-transformer
   if-transformer
   let-transformer
   letrec-transformer
   let-syntax-transformer
   letrec-syntax-transformer
   syntax-transformer
   )
  (import (rnrs) (rnrs eval)
          (util record-match) (util typed-records)
          (types) (s-exprs)
          )

;; ==============
;; == Overview ==
;; ==============
;;
;; This library implements the main expander and can be divided into
;; three parts: traversal, macro expansion and core form expansion.
;; The traversal find a place at which to perform an expansion.  Macro
;; expansion implements the expansion of user defined macros.  Core
;; form expansion handles the expansion of core forms such as lambda,
;; if and let.
;;
;; The traversal is what you would naively expect and doesn't do
;; anything interesting with respect to hygiene.  However, macro
;; expansion must ensure that macro transformers are "equivarient" in
;; order to preserve binder hygiene.  Finally, core form expansion
;; must freshly generate reference atoms and use the 'subst' function
;; in order to preserve reference hygiene.
;;
;; See the rest of this file for more detailed explanations on these
;; parts and how they interact.


;; ===============
;; == Traversal ==
;; ===============
;;
;; The expand-s-expr, expand-k-syntax*, expand-k-syntax and
;; expand-u-syntax functions are driver and traversal functions that
;; direct the overall process of expansion.  Nothing interesting with
;; respect to hygiene happens in these functions.
;;
;; The expand-u-syntax function examines a piece of u-syntax and
;; determines what core-form or macro transformer should be applied to
;; it.  (See "Core form expansion" and "macro expansion".)  It does
;; this by consulting the current environment.
;;
;; The expand-k-syntax function traverses a k-syntax to find a
;; u-syntax to which to apply expand-u-syntax.  Along the way it keeps
;; track of the current environment and any macro bindings introduced
;; by let-syntax or letrec-syntax forms.  But aside from that it is a
;; standard, boilerplate traversal.
;;
;; The expand-k-syntax* function repeatedly applies expand-k-syntax
;; until an iteration bound is met or there is no more expansion to be
;; done.
;;
;; The expand-s-expr function provides a simplified interface to
;; expand-k-syntax* by taking s-expressions as input and output.

;; ------------------
;; -- Environments --
;; ------------------
;;
;; The default environment, 'env0' is defined at the end of this file,
;; but since environments are used in several places it is worth
;; defining their structure here.
;;
;; In the expander, the environment maps symbols corresponding to the
;; names of ref-atoms to either a transformer function or #f.  We use
;; symbols stored in ref-atoms instead of ref-atoms for the key so we
;; can use 'assq' for environment lookup.
;;
;; A transformer function is a function from u-syntax (representing
;; the thing being expanded) to k-syntax (representing the result of
;; expansion).  If something maps to #f, then the variable is either
;; out of phase or out of context and thus cannot be used during
;; expansion.

;; -----------------------
;; -- Top-level drivers --
;; -----------------------

;; The expand-s-expr function acts as a wrapper for the expander.  It
;; goes from s-expr to s-expr.
;;
;; The 'n' parameter is the maximum number of expansion steps to take
;; or #f for no limit.
;;
;; The 'env' paramter is the initial environment and should usually be
;; env0.

(define (expand-s-expr n env s-expr)
  (s-expr-prettify-idents
   (k-syntax->s-expr
    (expand-k-syntax* n env (s-expr->k-syntax s-expr)))))

;; The expand-k-syntax* function is the same as expand-s-expr except
;; that it goes from k-syntax to k-syntax.  It is called by
;; expand-s-expr and its 'n' and 'env' parameters are the same as for
;; expand-s-expr.

(define (expand-k-syntax* n env k-syntax)
  (if (and n (= n 0))
      k-syntax
      (let ([k-syntax^ (expand-k-syntax env k-syntax)])
        (if (not k-syntax^)
            k-syntax
            (expand-k-syntax* (and n (- n 1)) env k-syntax^)))))

;; ------------------------
;; -- Expanding K-Syntax --
;; ------------------------

;; The expand-k-syntax function does one step of expansion for
;; k-syntax.  If there is not expansion to be done, it returns #f.
;;
;; Despite the length of the implementation of this function, it is
;; mostly boilerplate traversal code.  The only non-boilerplate part
;; is that it keeps track of the environment as it goes and the base
;; case calls expand-u-syntax.

(define (expand-k-syntax env0 k-syntax)
  ;; rec0 is the main recursion for doing the traversal.  In order to
  ;; cut off traversal once a u-syntax has expanded, we check the
  ;; 'done' boolean on each recursion.  Since the involves a side
  ;; effect, order of evaluation matters, so we have the 'seq' macro,
  ;; which is function application that evaluates left to right, and
  ;; the map-seq function, which is a variant of 'map' that traverses
  ;; the list left to right.  Also, since the let, letrec, let-syntax
  ;; and letrec-syntax forms have such similar code, we factor out
  ;; that code into rec-let0.
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
          [#(k-syntax-quote _) k-syntax]
          [#(k-const _) k-syntax]
          [#(k-var _) k-syntax]
          [#(k-lam args body) (seq (make-k-lam) args (map-seq rec body))]
          [#(k-app fun args) (seq (make-k-app) (rec fun) (map-seq rec args))]
          [#(k-if test true false) (seq (make-k-if) (rec test) (rec true) (rec false))])))

  ;; 'rec-let0' implements traversal on let-style forms.  This
  ;; involves extending the environment and so takes a little bit of
  ;; code.  The 'make' parameter is the k-syntax constructor for the
  ;; form we are processing.  The 'form' parameter is a symbol telling
  ;; us what form we are processing so we know the binding structure
  ;; to use.  The 'bindings' and 'body' parameters are the bindings
  ;; and body from the let-style form being traversed.
  (define (rec-let0 env make form bindings body)
    ;; extend-env extends 'env' with 'bindings'.  It uses 'f' to
    ;; create the values bound by 'bindings'.
    (define (extend-env f)
      (append (map (lambda (binding)
                     (cons (ref-atom-name (car binding))
                           (f (cadr binding)))) bindings) env))
    (define (rhs-env)
      (case form
        ;; The RHS of a let and let-syntax do not extend their
        ;; environments so we just return 'env'.
        [(let let-syntax) env]
        ;; The RHS of letrec and letrec-syntax extend their
        ;; environments in their RHS but evaluation of those RHS must
        ;; not use those new bindings.  (R6RS isn't clear about this
        ;; for letrec-syntax, but it seems the only sensible
        ;; interpretation.)  Thus we extend 'env' with #f bindings.
        [(letrec letrec-syntax) (extend-env (lambda (x) #f))]))
    (define (body-env)
      (case form
        ;; The body of a let or letrec gets an extended environment,
        ;; but since those are run-time values, they must not be used
        ;; when macro expanding.  Thus we extend 'env' with #f
        ;; bindings.
        [(let letrec) (extend-env (lambda (x) #f))]
        ;; The body of let-syntax and letrec-syntax are extended with
        ;; the bound macro transformers.
        [(let-syntax letrec-syntax) (extend-env make-macro-transformer)]))

    ;; We construct a k-syntax using 'make' but we traverse 'bindings'
    ;; with the new rhs-env and 'body' with the new body-env.  Note
    ;; that if a step of expansion happens in 'bindings', then they
    ;; may not be fully expanded yet.  In that case, we cannot
    ;; construct body-env.  Since 'done' being true results in not
    ;; changing the 'body' anyway, we skip the traversal and return
    ;; 'body' in that case.
    (define (rec env) (lambda (k-syntax) (rec0 env k-syntax)))
    (seq (make) (map-bindings (rec (rhs-env)) bindings)
                (if done body (map-seq (rec (body-env)) body))))

  ;; 'seq' is function application that evaluates left to right.
  ;; Example: (seq (a) b c) is equivalent to (a b c) but ensures that
  ;; b and c are evaluated left to right
  (define-syntax seq
    (syntax-rules ()
      [(_ (v ...) e0 e* ...) (let ([tmp e0]) (seq (v ... tmp) e* ...))]
      [(_ (v ...)) (v ...)]))

  ;; 'map-seq' is 'map' but it evaluates the mapped values left to right
  (define (map-seq f xs)
    (cond
     [(null? xs) '()]
     [else (seq (cons) (f (car xs)) (map-seq f (cdr xs)))]))

  ;; If 'done' is not true, then no expansion happened and we return
  ;; #f.  Otherwise, return the expanded result.
  (let ([x (rec0 env0 k-syntax)])
    (if done x #f)))

;; ------------------------
;; -- Expanding U-Syntax --
;; ------------------------

;; The expand-u-syntax function implements one step of expansion for
;; u-syntax.  Expansion of constants, variables, and function
;; applications is done directly, but forms that look like macro calls
;; are all dispatched to the corresponding transformer in the
;; environment.  These forms include not just macro calls but also
;; things like lambda, if, let, letrec, and syntax.  Structuring the
;; code this way means that locally defined macros can shadow these
;; forms.

(define (expand-u-syntax env u-syntax)
  (match u-syntax ()
   ;; Constants
   [c (constant? c) (make-k-const c)]
   ;; Variables
   [#(ident r b)
    (let ([m (assq r env)])
      (cond
       [(and m (cdr m)) (error 'expand-u-syntax "out of phase or context" r m)]
       [else (make-k-var r)]))]
   ;; Macro calls and core forms
   [(#(ident #(ref-atom r) b) . args)
    (assq r env)
    (let ([m (assq r env)])
      (cond
       [(not m) (error 'expand-u-syntax "unbound variable" r)]
       [(not (cdr m)) (error 'expand-u-syntax "out of phase or context" r m)]
       [else ((cdr m) u-syntax)]))]
   ;; Function calls
   [(fun . args) (make-k-app (make-k-u-syntax fun) (map make-k-u-syntax args))]))


;; =====================
;; == Macro expansion ==
;; =====================
;;
;; For macro expansion, the make-macro-transformer function takes a
;; fully expanded k-syntax that represented the code of a macro
;; transformer and generates a Scheme function implementing the
;; transformer.  Naively, this is essentially just 'eval', but that
;; would result in a non-hygienic expander.  The hyg function gets
;; around this problem.  It takes a non-hygenic macro transformer
;; function and returns a hygenic version of it.  This technique is
;; what enforces binder hygiene.

(define (make-macro-transformer k-syntax)
  (define f (eval (k-syntax->s-expr #t k-syntax) eval-environment))
  ;; Note that 'f' is not hygenic so we use 'hyg' to make it hygenic.
  ;; Also, u-syntax in the following lambda is the code for the macro
  ;; call while the k-syntax parameter to 'make-macro-transformer' is
  ;; the code for the macro definition.
  (lambda (u-syntax) (make-k-u-syntax ((hyg f) u-syntax))))

;; The environment to use when eval'ing the definition of a macro.
;; This might need to change if anything is added to the
;; expand-primitives library.
(define eval-environment
  (environment '(except (rnrs) free-identifier=? bound-identifier=?)
               '(expand-primitives)))

;; ------------------------
;; -- The 'hyg' function --
;; ------------------------

;; The 'hyg' function takes a function 'f' and returns a hygienic
;; version of it.  This is the critical part that implements binder
;; hygiene.  See the paper for more details.

(define (hyg f)
  (lambda (u-syntax)
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

    ;; Given the argument 'u-syntax', we want to find a permutation
    ;; 'perm' that will map every binder atom in 'u-syntax' to a fresh
    ;; binder atom.  We use 'u-syntax-map-idents' to apply 'make-perm'
    ;; to every identifier in 'u-syntax'.  'make-perm' then adds
    ;; mappings to perm that swap the binder parts of those
    ;; identifiers with freshly generated bind atoms.
    (u-syntax-map-idents make-perm u-syntax)

    ;; Once we have generated that permutation, we would normally
    ;; apply it to 'f', but since we can't apply a permutation
    ;; to a Scheme function, we take advantage of the identity
    ;;
    ;;   (perm f) x = perm (f (inv-perm x))
    ;;
    ;; where inv-perm is the inverse of perm.  Since the permutation
    ;; we generated is a self inverse, inv-perm is just perm.
    ;;
    ;; The result of all this is that the function returned by 'hyg'
    ;; is a hygenic function from u-syntax to u-syntax.
    (apply-permutation perm (f (apply-permutation perm u-syntax)))))


;; =========================
;; == Core form expansion ==
;; =========================
;;
;; The transformers for core forms convert u-syntax into the k-syntax
;; for the corresponding core forms.  The pattern matching for this is
;; a little fiddly but is basically what you would intuitively expect.
;; The interesting part is that when we expand into a core form that
;; does binding, we take the identifiers in binding positions and
;; replace them with freshly generated reference atoms.  We then take
;; the binding parts of those identifiers, and and scan through the
;; parts of the u-syntax in the scope of those identifiers.  As we do
;; so, for any identifiers with one of those binder parts, we replace
;; their reference part with the freshly generated reference atom.
;; This replacement process is done by subst.  The combination of
;; freshly generating atoms and using subst accomplishes reference
;; hygiene.

;; -----------
;; -- Subst --
;; -----------

;; subst traverses 'u-syntax', and for any identifiers with a binder
;; part equal to a bind-atom in bs, replaces their reference parts
;; with the corresponding ref-atoms in rs.  This is used by core forms
;; that do binding to bind the identifiers that are being bound to the
;; reference atoms freshly generated for the core form.

(define (subst bs rs u-syntax)
  (define mapping (map cons bs rs))
  (define (f ident)
    (define (matches? k) (bind-atom-equal? k (ident-bind ident)))
    (define (new-ident x) (make-ident (cdr x) (car x)))
    (cond
     [(assp matches? mapping) => new-ident]
     [else ident]))
  (u-syntax-map-idents f u-syntax))

;; ----------------------------
;; -- Core-form transformers --
;; ----------------------------

;; To expand a lambda form we simply generate some fresh ref-atoms and
;; use 'subst' to rebind the identifiers in the body to point to those
;; freshly generated ref-atoms.

(define-match (lambda-transformer (_ (#(ident r b) ...) . body*))
  (let ([r^ (map gensym-ref-atom r)])
    (make-k-lam r^ (map (lambda (body) (make-k-u-syntax (subst b r^ body))) body*))))

;; To expand an 'if' form, we just create the k-syntax object.
(define-match (if-transformer (_ test true false))
  (make-k-if (make-k-u-syntax test) (make-k-u-syntax true) (make-k-u-syntax false)))

;; To expand a 'syntax' quotation form, we just create the k-syntax object
(define-match (syntax-transformer (_ body))
  (make-k-syntax-quote body))

;; All of the let-style forms share similar code so we factor that
;; common code into the following macro.  The only differences between
;; forms are in the name of the transformer, the k-syntax constructor
;; for constructing the core form and whether or not it is a recursive
;; binding form.
(define-syntax define-let-style-transformer
  (syntax-rules ()
    [(_ name make recursive-bindings?)
     (define-match (name (_ ([#(ident r b) rhs] (... ...)) . body))
       ;; For each binding that we introduce, we create a fresh ref-atom
       (let ([r^ (map gensym-ref-atom r)])
         ;; In the rhs, we use subst to rebind the identifiers if we are a recursive form
         (define (binding-fun lhs rhs)
           (list lhs (make-k-u-syntax (if recursive-bindings? (subst b r^ rhs) rhs))))
         ;; In the body, we always use subst to rebind the identifiers
         (define (body-fun body) (make-k-u-syntax (subst b r^ body)))
         (make (map binding-fun r^ rhs) (map body-fun body))))]))

(define-let-style-transformer let-transformer make-k-let #f)
(define-let-style-transformer let-syntax-transformer make-k-let-syntax #f)
(define-let-style-transformer letrec-transformer make-k-letrec #t)
(define-let-style-transformer letrec-syntax-transformer make-k-letrec-syntax #t)

;; ------------------
;; -- Environments --
;; ------------------

;; env0 is the default environment and includes bindings for each core
;; form.  See the 'Environments' section at the top of this file for a
;; description of how environments work.

(define env0
  `((lambda . ,lambda-transformer)
    (if . ,if-transformer)
    (syntax . ,syntax-transformer)
    (let . ,let-transformer)
    (letrec . ,letrec-transformer)
    (let-syntax . ,let-syntax-transformer)
    (letrec-syntax . ,letrec-syntax-transformer)
    ))

)

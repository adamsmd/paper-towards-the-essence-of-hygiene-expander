(library (k-syntax)
  (export k-syntax k-syntax?
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

          traverse-k-syntax
          )
  (import (rnrs) (record-match) (typed-records)
          (atoms) (u-syntax))

;; This library defines types and operations for k-syntax, which
;; represents code that is already fully expanded and thus has a known
;; binding structure.

;;;;;;;;;;;;;;;;
;; K-syntax data type

(define-record-type k-syntax)
(define-typed-subrecord k-syntax k-const (value constant?))
(define-typed-subrecord k-syntax k-var (ident ref-atom?))
(define-typed-subrecord k-syntax k-lam (args (list-of? ref-atom?)) (body (list-of? k-syntax?)))
(define-typed-subrecord k-syntax k-app (fun k-syntax?) (args (list-of? k-syntax?)))
(define-typed-subrecord k-syntax k-if (test k-syntax?) (true k-syntax?) (false k-syntax?))
(define-typed-subrecord k-syntax k-let (bindings bindings?) (body (list-of? k-syntax?)))
(define-typed-subrecord k-syntax k-letrec (bindings bindings?) (body (list-of? k-syntax?)))
(define-typed-subrecord k-syntax k-let-syntax (bindings bindings?) (body (list-of? k-syntax?)))
(define-typed-subrecord k-syntax k-letrec-syntax (bindings bindings?) (body (list-of? k-syntax?)))
(define-typed-subrecord k-syntax k-syn (value u-syntax?))
(define-typed-subrecord k-syntax k-u-syntax (value u-syntax?))

(define (binding? x) (and (list? x) (eq? 2 (length x)) (ref-atom? (car x)) (k-syntax? (cadr x))))
(define bindings? (list-of? binding?))

;;;;;;;;;;;;;;;;
;; K-syntax traversal

;; Finds the left-most k-u-syntax and apply u-syntax-fun to it.
;; If there is none to update, returns #f.
;; Maintains an environment and uses env-fun to update it.
;;
;; u-syntax-fun : u-syntax? -> k-syntax?
;;
;; (env-fun form on-rhs lhs rhs env) -> env
;;   Must add one binding to env based on the given parameters
;;
;;   - 'form': Specifies which type of binding form triggered this
;;     call.  One of 'let 'letrec 'let-syntax or 'letrec-syntax
;;
;;   - 'on-rhs': #t if we are extending the environment for the rhs of
;;     the bindings in a binding form.  #f if we are extending the
;;     environment for the body of a binding form.
;;
;;   - 'lhs': The name of a ref-atom for a binding to add.
;;
;;   - 'rhs': The k-syntax for a binding to add.
;;
;;   - 'env': An association list mapping names of ref-atoms to either
;;     #f (for out of phase or context variables) or a k-syntax.
;;
;; Note that we use 'ap' and 'ap-map' to help us manage the job of
;; continuing the traversal when recursive calls return #f but
;; stopping when they return a true value.
(define (traverse-k-syntax u-syntax-fun env-fun)
  (define (rec-env env k-syntax)

    (define (extend-env form on-rhs bindings)
      (define (extend binding env)
        (env-fun form on-rhs (ref-atom-name (car binding)) (cadr binding) env))
      (fold-right extend env bindings))
    (define (rec-bindings form bindings)
      (define env^ (extend-env form #t bindings))
      (ap-map (lambda (binding) (ap (list (car binding)) (rec-env env^ (cadr binding)))) bindings))
    (define (rec-body form bindings body)
      (define env^ (extend-env form #f bindings))
      (ap-map (lambda (k-syntax) (rec-env env^ k-syntax)) body))

    (define (rec k-syntax) (rec-env env k-syntax))
    (match k-syntax ()
     ;; Apply "u-syntax-fun" to the u-syntax
     [#(k-u-syntax value) (u-syntax-fun env value)]

     ;; Boilerplate recursions
     [#(k-const _) #f]
     [#(k-var _) #f]
     [#(k-lam args body) (ap (make-k-lam args) (ap-map rec body))]
     [#(k-app fun args) (ap (make-k-app) (rec fun) (ap-map rec args))]
     [#(k-if test true false) (ap (make-k-if) (rec test) (rec true) (rec false))]

     ;; Binding forms
     [#(k-let bindings body)
      (ap (make-k-let)
          (rec-bindings 'let bindings)
          (rec-body 'let bindings body))]
     [#(k-letrec bindings body)
      (ap (make-k-letrec)
          (rec-bindings 'letrec bindings)
          (rec-body 'letrec bindings body))]
     [#(k-let-syntax bindings body)
      (ap (make-k-let-syntax)
          (rec-bindings 'let-syntax bindings)
          (rec-body 'let-syntax bindings body))]
     [#(k-letrec-syntax bindings body)
      (ap (make-k-letrec-syntax)
          (rec-bindings 'letrec-syntax bindings)
          (rec-body 'letrec-syntax bindings body))]

     ;; Primitives
     [#(k-syn syn) #f]))

  rec-env)

;;;;;;;;;;;;;;;;
;; Helpers for 'traverse-k-syntax'.

;; This macro helps find where to stop a traversal by
;; checking when a recursive call returns #f which means
;; that there are no changes to be made.
;;
;; For example suppose we have the call:
;;   (ap (fun a b) (f i j) (g x y))
;; This is morally equivalent to:
;;   (fun a b (f i j) (g x y))
;; Except that we check whether the calls to f and g return #f.
;;
;; If (f i j) is not #f, then we return:
;;    (fun a b (f i j) y)
;; Note that we do not recur on y and thus leave it unchanged.
;;
;; If (f i j) is #f but (g x y) is not, we return:
;;   (fun a b j (g x y))
;;
;; Finally if both f and g return #f, we return #f.
(define-syntax ap
  (syntax-rules ()
    [(_ (fun ...)) #f]
    [(_ (fun ...) (m0 ... d0) (m ... d) ...)
     (let ([a (m0 ... d0)])
       (if a
           (fun ... a d ...)
           (ap (fun ... d0) (m ... d) ...)))]))

;; This is a list version of 'ap'.  It applied 'f' to each element of
;; 'ls' until we find an element that does not return #f.  The list is
;; then returned with that element replaced by the return value of 'f'.
(define (ap-map f ls)
  (define (rec head tail)
    (cond
     [(null? tail) #f]
     [else (let ([a (f (car tail))])
             (if a
                 (append head (cons a (cdr tail)))
                 (rec (append head (list (car tail))) (cdr tail))))]))
  (rec '() ls))

)

(library (util record-match-helpers)
  (export match-pattern pattern-vars syntax->list curry escape-ellipses)
  (import (rnrs))

;; This is a helper library used by record-match.scm for implementing
;; match.  Users should never need to directly use anything in this
;; library.

;; ---------------------
;; -- Utility helpers --
;; ---------------------

;; Returns a list of integers from n-1 down to 0
(define (rev-iota n)
  (cond
   [(zero? n) '()]
   [else (cons (- n 1) (rev-iota (- n 1)))]))

;; Returns a list of integers from 0 up to n-1
(define (iota n) (reverse (rev-iota n)))

;; Curries the function 'f'
(define (curry f . args1) (lambda args2 (apply f (append args1 args2))))

;; Returns true if k is free-identifier=? to some element of xs
(define (mem-free-identifier k xs) (exists (curry free-identifier=? k) xs))

;; Returns true if k is bound-identifier=? to some element of xs
(define (mem-bound-identifier k xs) (exists (curry bound-identifier=? k) xs))

;; Converts a syntax object representing a list into a list of syntax objects
(define (syntax->list xs)
  (syntax-case xs ()
    [() '()]
    [(x . xs) (cons #'x (syntax->list #'xs))]))

;; Takes a list of identifiers and removes the duplicates (as defined by bound-identifier=?)
(define (uniq xs)
  (cond
   [(null? xs) '()]
   [(mem-bound-identifier (car xs) (cdr xs)) (uniq (cdr xs))]
   [else (cons (car xs) (uniq (cdr xs)))]))

;; Converts any ellipses in a syntax object into double ellipses (which is an escaping form for ellipses)
(define (escape-ellipses stx)
  (syntax-case stx ()
    [x (and (identifier? #'x) (free-identifier=? #'x #'(... ...))) #'((... ...) (... ...))]
    [(x . y) #`(#,(escape-ellipses #'x) . #,(escape-ellipses #'y))]
    [#(x ...) (with-syntax ([(y ...) (escape-ellipses #'(x ...))]) #`#(y ...))]
    [_ stx]))

;; Returns a list of the variables bound by a pattern
;;
;; Example: (pattern-vars (list #'lit) #'(x (#(foo y z) (... ...)) lit 3 _))
;;   ==> (#<syntax x> #<syntax y> #<syntax z>)
(define (pattern-vars lits pattern)
  (define (rec pattern)
    (syntax-case pattern ()
      [id (and (identifier? #'id) (free-identifier=? #'id #'_)) '()]
      [id (and (identifier? #'id) (mem-free-identifier #'id lits)) '()]
      [id (identifier? #'id) `(,#'id)]
      [(pat dots) (and (identifier? #'dots) (free-identifier=? #'dots #'(... ...))) (rec #'pat)]
      [(pat-car . pat-cdr) (append (rec #'pat-car) (rec #'pat-cdr))]
      [#(name fields ...)
       (letrec ([f (lambda (fields)
                     (syntax-case fields ()
                       [() '()]
                       [(car . cdr) (append (rec #'car) (f #'cdr))]))])
         (f #'(fields ...)))]
      [atom (let ([x (syntax->datum #'atom)])
              (or (boolean? x) (number? x) (char? x)
                  (string? x) (null? x)))
            '()]))
  (uniq (rec pattern)))

;; This function takes a list of environments mapping keys to results
;; and produces a new environment that maps keys to lists of results.
;; It is used to implement "..." in the matcher.
(define (combine-envs envs)
  (define (get key)
    (cons key
      (fold-right (lambda (env env*)
                    (cons (cdr (assp (curry bound-identifier=? key) env)) env*)) '() envs)))
  (map get (map car (car envs))))

;; 'match-pattern' is a simple interpreter that matches a datum
;; against a pattern.  If it matches, an association list is returned
;; with the bindings for each pattern variable.  Otherwise it returns
;; #f.  This function is the core of the pattern matcher and is
;; basically an interpreter for the pattern language.
;;
;; Note: The returned env is always fully populated with all pattern variables
;;
;; Example:
;;   > (define-record-type foo (fields a b))
;;   > (match-pattern (list #'lit) #'(x (#(foo y z) (... ...)) lit 3 _) `(1 (,(make-foo 2 3) ,(make-foo 4 5)) lit 3 9))
;;   ((#<syntax x> . 1) (#<syntax y> 2 4) (#<syntax z> 3 5))

(define (match-pattern lits pattern datum)
  (define (go k)
    (define (fail) (k #f))
    (define-syntax test
      (syntax-rules () [(_ pred body) (if pred body (fail))]))
    (define (rec pattern datum)
      (syntax-case pattern ()
        [id (and (identifier? #'id) (free-identifier=? #'id #'_)) '()]
        [id (and (identifier? #'id) (mem-free-identifier #'id lits)) (test (equal? datum (syntax->datum #'id)) '())]
        [id (identifier? #'id) `((,#'id . ,datum))]
        [(pat dots) (and (identifier? #'dots) (free-identifier=? #'dots #'(... ...)))
         (test (list? datum)
               (if (null? datum)
                   (map (lambda (k) `(,k . ())) (pattern-vars pattern))
                   (combine-envs (map (curry rec #'pat) datum))))]
        [(pat-car . pat-cdr)
         (test (pair? datum)
               (append (rec #'pat-car (car datum)) (rec #'pat-cdr (cdr datum))))]
        [#(name fields ...)
         (test (and (record? datum)
                    (equal? (syntax->datum #'name) (record-type-name (record-rtd datum))))
               (let ([rtd (record-rtd datum)])
                 (assert (equal? (length (syntax->datum #'(fields ...)))
                                 (vector-length (record-type-field-names rtd))))
                 (apply append (map (lambda (field index) (rec field ((record-accessor rtd index) datum)))
                                    #'(fields ...) (iota (length (syntax->datum #'(fields ...))))))))]
        [atom (let ([x (syntax->datum #'atom)])
                (or (boolean? x) (number? x) (char? x)
                    (string? x) (null? x)))
              (test (equal? datum (syntax->datum #'atom)) '())]
        [_ (syntax-violation 'match "unknown pattern form" pattern)]))
    (rec pattern datum))
  (call/cc go))

)

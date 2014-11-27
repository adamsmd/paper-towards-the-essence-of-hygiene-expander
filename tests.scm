(import (expand) (gensym))

;; This file contains tests for the macro expander

;;;;;;;;;;;;;;;;
;; Testing framework

;; Returns true if two s-expr are alpha equivalent
(define (alpha-eq? s1 s2)
  (define perm '())
  (define (rec s1 s2)
    (or
     (eq? s1 s2)
     (and (pair? s1) (pair? s2)
          (rec (car s1) (car s2))
          (rec (cdr s1) (cdr s2)))
     (and (vector? s1) (vector? s2)
          (= (vector-length s1) (vector-length s2))
          (for-all rec (vector->list s1) (vector->list s2)))
     (and (symbol? s1) (symbol? s2)
          (cond
           [(assq s1 perm) (eq? (cdr (assq s1 perm)) s2)]
           [(and (gensym? s1) (gensym? s2))
            (set! perm (cons (cons s1 s2) perm)) #t]
           [else #f]
           ))))
  (rec s1 s2))

;; A record storing the information needed to run a test
(define-record-type test (fields name input output))

;; A list of tests that are defined
(define tests '())

;; Defines a test
(define-syntax define-test
  (syntax-rules ()
    [(_ name input output)
     (begin
       (define name (make-test 'name input output))
       (set! tests (cons name tests)))]))

;; Runs all tests that have been defined
(define (run-tests)
  (for-each (lambda (test)
              (display "Running '") (display (test-name test)) (display "'") (newline)
              (if (not (alpha-eq? (test-output test)
                                  (expand-s-expr #f env0 (test-input test))))
                  (begin (display "!!! FAILED !!!") (newline))))
            (reverse tests)))

;; Runs a single test with extra information about input and output
(define (run-test test)
  (display "Testing: ")(display (test-name test))(newline)
  (display "Running: ")(pretty-print `(expand-s-expr #f env0 (test-input ,(test-name test))))
  (display "Input:")(newline)
  (pretty-print (test-input test))
  (display "Expected output:")(newline)
  (pretty-print (test-output test))
  (let ([actual (expand-s-expr #f env0 (test-input test))])
    (display "Actual output:")(newline)
    (pretty-print (expand-s-expr #f env0 (test-input test)))
    (display "Output comparison: ")(write (alpha-eq? (test-output test) actual))(newline)))

;;;;;;;;;;;;;;;;
;; Test helpers

;; Helper for building tests involving the 'or' macro
(define (or-macro body)
  `(letrec-syntax ([or (lambda (stx)
                         (if (null? (cdr stx))
                             #'#t
                             (if (null? (cddr stx))
                                 (cadr stx)
                                 (list #'let (list (list #'tmp (cadr stx)))
                                       (list #'if #'tmp #'tmp (cons #'or (cddr stx)))))))])
     ,body))

;; Helper for building tests involving the 'let-inc' macro
(define (let-inc-macro body)
  `(let-syntax ([let-inc (lambda (stx)
                           (let ([u (cadr stx)]
                                 [v (caddr stx)])
                             (list #'let (list (list u (list #'+ #'1 u))) v)))])
     ,body))

;; Helper for building tests involving the 'm' macro
(define (m-macro body)
  `(let-syntax ([m (lambda (stx)
                     (let ([y (cadr stx)])
                       (list #'let-inc #'x (list #'* #'x #'y))))])
     ,body))

;;;;;;;;;;;;;;;;
;; Tests of the 'or' macro

(define-test or-test0 (or-macro `(or 1 2))
  '(letrec-syntax ([or@7 (lambda (stx@8)
                           (if (null? (cdr stx@8))
                               #'#t
                               (if (null? (cddr stx@8))
                                   (cadr stx@8)
                                   (list
                                    #'#(let let)
                                    (list (list #'#(tmp tmp) (cadr stx@8)))
                                    (list
                                     #'#(if if)
                                     #'#(tmp tmp)
                                     #'#(tmp tmp)
                                     (cons #'#(or@7 or) (cddr stx@8)))))))])
     (let ([tmp@9 '1]) (if tmp@9 tmp@9 '2))))
(define-test or-test1 `(let ([tmp 1]) ,(or-macro `(or 2 tmp)))
  '(let ([tmp@7 '1])
     (letrec-syntax ([or@8 (lambda (stx@9)
                             (if (null? (cdr stx@9))
                                 #'#t
                                 (if (null? (cddr stx@9))
                                     (cadr stx@9)
                                     (list
                                      #'#(let let)
                                      (list
                                       (list #'#(tmp@7 tmp) (cadr stx@9)))
                                      (list
                                       #'#(if if)
                                       #'#(tmp@7 tmp)
                                       #'#(tmp@7 tmp)
                                       (cons
                                        #'#(or@8 or)
                                        (cddr stx@9)))))))])
       (let ([tmp@10 '2]) (if tmp@10 tmp@10 tmp@7)))))
(define-test or-test2 (or-macro `(let ([tmp 1]) (or 2 tmp)))
  '(letrec-syntax ([or@11 (lambda (stx@12)
                            (if (null? (cdr stx@12))
                                #'#t
                                (if (null? (cddr stx@12))
                                    (cadr stx@12)
                                    (list
                                     #'#(let let)
                                     (list (list #'#(tmp tmp) (cadr stx@12)))
                                     (list
                                      #'#(if if)
                                      #'#(tmp tmp)
                                      #'#(tmp tmp)
                                      (cons
                                       #'#(or@11 or)
                                       (cddr stx@12)))))))])
     (let ([tmp@13 '1])
       (let ([tmp@14 '2]) (if tmp@14 tmp@14 tmp@13)))))

(define or-test3 `(let ([if 1]) ,(or-macro `(or 2 if)))) ;; TODO@ automate the check that this thows an error

(define-test or-test4 (or-macro `(let ([if 1]) (or 2 if)))
  '(letrec-syntax ([or@15 (lambda (stx@16)
                            (if (null? (cdr stx@16))
                                #'#t
                                (if (null? (cddr stx@16))
                                    (cadr stx@16)
                                    (list
                                     #'#(let let)
                                     (list (list #'#(tmp tmp) (cadr stx@16)))
                                     (list
                                      #'#(if if)
                                      #'#(tmp tmp)
                                      #'#(tmp tmp)
                                      (cons
                                       #'#(or@15 or)
                                       (cddr stx@16)))))))])
     (let ([if@17 '1])
       (let ([tmp@18 '2]) (if tmp@18 tmp@18 if@17)))))

;;;;;;;;;;;;;;;;
;; Tests of the 'inc' macro

(define-test inc-test1 `(let ([x 3]) ,(let-inc-macro (m-macro `(m x))))
  '(let ([x@19 '3])
     (let-syntax ([let-inc@20 (lambda (stx@21)
                                (let ([u@22 (cadr stx@21)]
                                      [v@23 (caddr stx@21)])
                                  (list
                                   #'#(let let)
                                   (list (list u@22 (list #'#(+ +) #'1 u@22)))
                                   v@23)))])
       (let-syntax ([m@24 (lambda (stx@25)
                            (let ([y@26 (cadr stx@25)])
                              (list
                               #'#(let-inc@20 let-inc)
                               #'#(x@19 x)
                               (list #'#(* *) #'#(x@19 x) #'#(y@26 y)))))])
         (let ([x@27 (+ '1 x@19)]) (* x@27 y@26))))))

(run-tests)

;; TODO:
;;  - Environment handling in expand-k-syntax
;;  + The macro case in expand-u-syntax
;;  - Simplify 'match' notation
;;  + Library implementing syntax-case, free-identifier=?
;;  - Check for duplicate identifers in binding lists
;;  - Pretty print k-syntax

;; all that is left is the environment handling in expand-k-syntax and environment lookup in expand-u-syntax


#|

(letrec-syntax ([m1 (lambda (x) x)]
                [m2 (m1 (lambda (x) x))])
3)

(let-syntax ([m1 (lambda (x) x)]
             [m2 (m1 (lambda (x) x))])
3)

(let ([x 1])
  (let-syntax ([m (lambda (y) (if x 2 3))])
    (m)))

|#


;  (match (where ([i ident?] [(r b) (and (ref-atom? r) (bind-atom? b) (ident? this))]
;  [c ...
;  [<r b> ..
;  [(<lambda _> ...
;; short hand for "match ... [(#(ident #(ref-atom foo) _) ...) ..]

#|

> (s-expr->k-syntax '(lambda (x) x))

> (k-syntax->s-expr (expand-k-syntax '() (expand-k-syntax '() (s-expr->k-syntax '(lambda (x) x)))))
(lambda (#{x stw3ewjeozscrb1a8f9bs-3})
  #{x stw3ewjeozscrb1a8f9bs-3})

> (k-syntax->s-expr (expand-k-syntax '() (expand-k-syntax '() (expand-k-syntax '() (expand-k-syntax '() (expand-k-syntax '() (s-expr->k-syntax '(let-syntax ([m (lambda (stx) (syntax z))]) (m)))))))))
(begin z)

> (expand-s-expr #f '() '(let-syntax ([m (lambda (stx) (syntax z))]) (m)))
(begin z)

;; macros:
;;  or
;;  let-inc / m

|#

(import (expand) (util gensym))

;; This file contains tests for the macro expander

;; =====================
;; == Test definition ==
;; =====================

;; List of tests that are defined
(define tests '())

;; Record type for storing the information needed to run a test
(define-record-type test (fields name input output))

;; Test definition
(define-syntax define-test
  (syntax-rules ()
    [(_ name input output)
     (begin
       (define name (make-test 'name input output))
       (set! tests (cons name tests)))]))

;; ===========
;; == Tests ==
;; ===========

;; ------------------
;; -- Simple tests --
;; ------------------

(define-test test0 `(lambda (x) x)
  `(lambda (x@1) x@1))

(define-test test1 `(let-syntax ([m (lambda (stx) (syntax z))]) (m))
  `(let-syntax ([m@1 (lambda (stx@2) (syntax #(z z)))]) z))

;; --------------------
;; -- Tests for 'or' --
;; --------------------

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

(define (or-macro-output tmp body)
  `(letrec-syntax ([or@7 (lambda (stx@8)
                           (if (null? (cdr stx@8))
                               #'#t
                               (if (null? (cddr stx@8))
                                   (cadr stx@8)
                                   (list #'#(let let) (list (list #'#(,tmp tmp) (cadr stx@8)))
                                         (list #'#(if if) #'#(,tmp tmp) #'#(,tmp tmp)
                                               (cons #'#(or@7 or) (cddr stx@8)))))))])
     ,body))

(define-test or-test0 (or-macro `(or 1 2))
  (or-macro-output 'tmp '(let ([tmp@9 '1]) (if tmp@9 tmp@9 '2))))
(define-test or-test1 `(let ([tmp 1]) ,(or-macro `(or 2 tmp)))
  `(let ([tmp@7 '1]) ,(or-macro-output 'tmp@7 '(let ([tmp@10 '2]) (if tmp@10 tmp@10 tmp@7)))))
(define-test or-test2 (or-macro `(let ([tmp 1]) (or 2 tmp)))
  (or-macro-output 'tmp '(let ([tmp@13 '1]) (let ([tmp@14 '2]) (if tmp@14 tmp@14 tmp@13)))))
(define-test or-test3 (or-macro `(let ([if 1]) (or 2 if)))
  (or-macro-output 'tmp '(let ([if@17 '1]) (let ([tmp@18 '2]) (if tmp@18 tmp@18 if@17)))))

;; (define or-test4 `(let ([if 1]) ,(or-macro `(or 2 if)))) ;; TODO: automate the check that this thows an error

;; ---------------------
;; -- Tests for 'inc' --
;; ---------------------

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


;; =======================
;; == Testing framework ==
;; =======================

;; Returns true if two s-expr are equivalent modulo gensym renaming
(define (renamable? s1 s2)
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
    (display "Output comparison: ")(write (renamable? (test-output test) actual))(newline)))

;; Runs all tests that have been defined
(define (run-tests)
  (for-each (lambda (test)
              (display "Running '") (display (test-name test)) (display "'") (newline)
              (if (not (renamable? (test-output test) (expand-s-expr #f env0 (test-input test))))
                  (begin (display "!!! FAILED !!!") (newline))))
            (reverse tests)))

(run-tests)

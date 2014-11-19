(library (atoms)
  (export
   ref-atom ref-atom? make-ref-atom ref-atom-name
   bind-atom bind-atom? make-bind-atom bind-atom-name
   ref-atom-equal? bind-atom-equal? 
   gensym-ref-atom gensym-bind-atom
   )
  (import (rnrs) (typed-records) (gensym))

;; This library defines types and operations for ref-atoms and bind-atoms

;; There are two types of atoms: ref-atom and bind-atom.  Each unique
;; atom is represented by a symbolic name.
(define-record-type atom)
(define-typed-subrecord atom ref-atom (name symbol?))
(define-typed-subrecord atom bind-atom (name symbol?))

;; ref-atom-equal? and bind-atom-equal? respectively test whether two
;; ref-atoms or bind-atoms have the name symbolic name.
(define (ref-atom-equal? r1 r2) (equal? (ref-atom-name r1) (ref-atom-name r2)))
(define (bind-atom-equal? b1 b2) (equal? (bind-atom-name b1) (bind-atom-name b2)))

;; gensym-ref-atom and gensym-bind-atom generate unique, fresh
;; versions of ref-atoms or bind-atoms, respectively
(define (gensym-ref-atom a) (make-ref-atom (gensym (ref-atom-name a))))
(define (gensym-bind-atom a) (make-bind-atom (gensym (bind-atom-name a))))

)

(library (expand-primitives)
  (export (rename (ident-ref=? free-identifier=?) (ident-bind=? bound-identifier=?)))
  (import (types))

;; This library defines the primitives that are accessible to macro
;; transformer code.  For now, the only things we export are our
;; implementations of free-identifer=? and bound-identifier=?  which
;; are implemented as ident-ref=? and ident-bind=? from the types
;; library.

;; If anything is added to this library, eval-environment in
;; expand.scm may need to be updated.

;; Some features that should be implemented in the future but are not
;; currently are:
;;  - syntax-case
;;  - (syntax ...)
;;  - (quasi-syntax ...)
;;  - (unquote-syntax ...)
)

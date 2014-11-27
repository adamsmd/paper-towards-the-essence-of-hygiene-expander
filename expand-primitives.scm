(library (expand-primitives)
  (export (rename (ident-ref=? free-identifier=?) (ident-bind=? bound-identifier=?)))
  (import (types))

;; This library defines the primitives that are accessible to macro
;; transformers when they are expanding.

;; Some features that should be implemented in the future but are not currently implemented are:
;;  - syntax-case
;;  - (syntax ...)
;;  - (quasi-syntax ...)
;;  - (unquote-syntax ...)
)

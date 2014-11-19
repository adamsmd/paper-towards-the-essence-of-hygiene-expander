(library (gensym)
  (export gensym gensym? prettify-gensym)
  (import (scheme))

;; We need a way to generate fresh symbols.  Since gensym is not
;; standardized, this library exports gensym and should be modified as
;; needed to implement gensym.

;; Imports from (scheme) that are re-exported
;;
;; gensym : string? -> symbol?
;; gensym? : object -> boolean?

;; Takes a gensym and returns a symbol with an easier to read name
(define (prettify-gensym s)
  (let ([str (gensym->unique-string s)])
    (define (suffix i)
      (cond
       [(= i (string-length str)) str]
       [(eq? '#\- (string-ref str i)) (substring str (+ i 1) (string-length str))]
       [else (suffix (+ i 1))]))
    (string->symbol (string-append (symbol->string s) ":" (suffix 0)))))

)

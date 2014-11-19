(library (gensym)
  (export gensym gensym?)
  (import (rnrs))

;; We need a way to generate fresh symbols.  Since gensym is not
;; standardized, this library manually implements gensym.  It assumes
;; that "@" is never used in a symbol.

;; The character reserved for use in gensyms
(define gensym-char '#\@)

;; The counter used internally to create new gensyms
(define count 0)

;; A local helper for finding characters in strings
(define (string-index str c i)
  (cond
   [(eq? i (string-length str)) i]
   [(eq? c (string-ref str i)) i]
   [else (string-index str c (+ i 1))]))

;; Creates a new gensym based on the symbol sym
(define (gensym sym)
  (set! count (+ 1 count))
  (let* ([s (symbol->string sym)]
         [i (string-index s gensym-char 0)])
    (string->symbol (string-append (substring s 0 i) (list->string (list gensym-char)) (number->string count)))))

;; Returns true iff sym is a gensym
(define (gensym? sym)
  (and (symbol? sym)
       (not (eq? (string-length (symbol->string sym)) (string-index (symbol->string sym) gensym-char 0)))))

)

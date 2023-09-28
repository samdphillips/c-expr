#lang racket/base

(require racket/unit
         "exn.rkt"
         "lexer-unit.rkt"
         "lexer-ext-sig.rkt"
         "lexer-sig.rkt"
         "token.rkt")

(provide (all-from-out "token.rkt"))
(provide-signature-elements lexer^)

(define (handle-unknown-lexeme lexeme start-pos input-port)
  (raise-read-error
   (format "No match found for input starting with: ~s" lexeme)
   (->srcloc input-port start-pos)))

(define-values/invoke-unit lexer@
  (import lexer-ext^)
  (export lexer^))


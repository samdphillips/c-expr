#lang racket/base

(require (prefix-in - syntax/readerr)
         parser-tools/lex
         racket/match
         syntax/srcloc)

(provide lex-c-expr
         (struct-out token)
         (struct-out id)
         (struct-out opener)
         (struct-out closer)
         )

(define (->srcloc source pos)
  (and pos
       (match-let ([(position offset line column) pos])
         (vector (if (input-port? source)
                     (object-name source)
                     source)
                 line column offset 0))))

(define ((make-raise-read-error raise-proc) message srcloc)
  (raise-proc message
              (source-location-source   srcloc)
              (source-location-line     srcloc)
              (source-location-column   srcloc)
              (source-location-position srcloc)
              (source-location-span     srcloc)))

(define raise-read-error
  (make-raise-read-error -raise-read-error))
(define raise-read-eof-error
  (make-raise-read-error -raise-read-eof-error))

(struct token (srcloc value) #:transparent)
(struct id token () #:transparent)
(struct opener token () #:transparent)
(struct closer token () #:transparent)

(define lex-c-expr
  (let ()
    (define-syntax-rule ($token maker value)
      (maker (build-source-location-vector
               (->srcloc input-port start-pos)
               (->srcloc input-port end-pos))
             value))
    (lexer
      [(eof) eof]
      [any-char (raise-read-error
                 (format "No match found for input starting with: ~s" lexeme)
                 (->srcloc input-port start-pos))]
  )))
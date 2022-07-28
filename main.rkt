#lang racket/base

#|
    https://docs.oracle.com/javase/specs/jls/se8/html/jls-3.html
|#

(require (prefix-in - syntax/readerr)
         (prefix-in : parser-tools/lex-sre)
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
      ;; whitespace
      [(:+ whitespace) (lex-c-expr input-port)]
      ;; comments
      [(:: "//" (:* (:~ #\newline))) (lex-c-expr input-port)]

      ;; identifiers
      [(:: (:or #\$ #\_ alphabetic)
           (:* (:or #\$ #\_ alphabetic numeric)))
       ($token id (string->symbol lexeme))]

      ;; openers
      [(:or #\( #\{ #\[)
       ($token opener lexeme)]

      ;; closers
      [(:or #\) #\} #\])
       ($token closer lexeme)]

      ;; error
      [any-char (raise-read-error
                 (format "No match found for input starting with: ~s" lexeme)
                 (->srcloc input-port start-pos))]
  )))
#lang racket/base

(require parser-tools/lex
         racket/match)

(provide ->srcloc
         (struct-out token))

(define (->srcloc source pos)
  (and pos
       (match-let ([(position offset line column) pos])
         (vector (if (input-port? source)
                     (object-name source)
                     source)
                 line column offset 0))))

(struct token (srcloc value) #:transparent)
(define-syntax-rule (define-token name)
  (begin (struct name token () #:transparent)
         (provide (struct-out name))))
(define-token id)
(define-token literal)
(define-token opener)
(define-token closer)
(define-token separator)
(define-token operator)

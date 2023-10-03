#lang racket/base

(require (for-syntax racket/base)
         parser-tools/lex
         racket/match
         syntax/parse/define)

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
(define-syntax-parse-rule (define-token name {~optional parent:id #:defaults [(parent #'token)]})
  (begin (struct name parent () #:transparent)
         (provide (struct-out name))))

(define-token separator)
(define-token major-separator separator)
(define-token minor-separator separator)

(define-token id)
(define-token literal)
(define-token opener)
(define-token closer)
(define-token operator)

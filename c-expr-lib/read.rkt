#lang racket/base

(require c-expr/private/reader
         racket/contract
         syntax/strip-context)
(provide (contract-out
           [read-c-expr        (-> input-port? any)]
           [read-c-expr-syntax (-> any/c input-port? (or/c eof-object? syntax?))]))

(define (read-c-expr inp)
  (syntax->datum
    (read-c-expr-syntax #f inp)))

(define (read-c-expr-syntax src inp)
  (read-top inp))

(module* reader #f
  (provide (contract-out
             [read (-> input-port? any)]
             [read-syntax (-> any/c input-port? (or/c eof-object? syntax?))]))

  (define (read inp)
    (syntax->datum
      (read-c-expr-syntax #f inp)))
  
  (define (read-syntax src inp)
    (with-syntax ([top (read-c-expr-syntax src inp)])
      (strip-context
        #'(module mod racket/base 'top)))))

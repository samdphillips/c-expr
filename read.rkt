#lang racket/base

(module reader racket/base
  (require c-expr/private/reader
           racket/contract
           syntax/strip-context)
  (provide (contract-out
             [read (-> input-port? any)]
             [read-syntax (-> any/c input-port? (or/c eof-object? syntax?))]))

  (define (read inp)
    (syntax->datum
      (read-syntax #f inp)))

  (define (read-syntax src inp)
    (with-syntax ([top (read-top inp)])
      (strip-context
        #'(module mod racket/base 'top)))))

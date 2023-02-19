#lang racket/base

(require (for-syntax racket/base)
         syntax/parse/define)

(provide (rename-out [module-begin #%module-begin]))

(define-syntax module-begin
  (syntax-parser
    #:datum-literals (top)
    [(_ (top . body)) #'(#%module-begin (example-top . body))]))

(define-syntax example-top
  (lambda (stx)
    (syntax-case stx () [(_ . xs) #'(quote xs)])))

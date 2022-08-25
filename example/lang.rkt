#lang racket/base

(require (for-syntax racket/base))

(provide #%module-begin
         top)

(define-syntax top
  (lambda (stx)
    (displayln stx)
    (syntax-case stx () [(_ . xs) #'(quote xs)])))

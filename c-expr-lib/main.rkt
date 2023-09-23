#lang racket/base

(module reader syntax/module-reader
  #:language    read
  #:read        read-c-expr
  #:read-syntax read-c-expr-syntax
  #:whole-body-readers? #t
  (require c-expr/read))

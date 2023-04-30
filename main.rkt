#lang racket/base

(module reader syntax/module-reader
  #:language    read
  #:read        read-c-expr
  #:read-syntax (lambda (src in)
                  (with-syntax ([body (read-c-expr-syntax src in)])
                    #'(body)))
  #:whole-body-readers? #t
  (require c-expr/read))

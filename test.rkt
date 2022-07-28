#lang at-exp racket/base

(require (for-syntax racket/base)
         c-expr
         racket/format
         racket/port
         racket/sequence
         rackunit
         syntax/parse/define)

(begin-for-syntax
  (define-syntax-class token-pat 
    [pattern (name:id srcloc-pat value-pat)]))

(define-syntax-parse-rule (test-lex name:str tokens:token-pat ... input:str ...)
  #:with (input0 . _) #'(input ...)
  #:do [(define (x f) (datum->syntax #'input0 (f #'input0) #'input0))]
  #:with line     (x syntax-line)
  #:with column   (x syntax-column)
  #:with position (x syntax-position)
  #:with source   (x (lambda (stx) (path->string (syntax-source stx))))
  (test-case name
    (check-match 
      (let ()
        (define inp0 (open-input-string (~a input ...)))
        (port-count-lines! inp0)
        (define inp
          (relocate-input-port inp0
                               line
                               column
                               position
                               #t
                               #:name source))
        (port-count-lines! inp)
        (define actual (sequence->list (in-port lex-c-expr inp)))
        (close-input-port inp)
        actual)
      (list tokens ...))))

(module* test #f
  @test-lex["basic lex"
            (id _ 'class) (id _ 'Foo) (opener _ "{") (closer _ "}")
            (id _ 'class) (id _ 'Foo2) (opener _ "{") (closer _ "}")]{
  // this is a comment
  class Foo { }
  class Foo2 { }
  }
)

#lang at-exp racket/base

(require (for-syntax racket/base)
         c-expr/private/lexer
         racket/format
         racket/port
         racket/sequence
         rackunit
         syntax/parse/define)

(begin-for-syntax
  (define-syntax-class token-pat 
    [pattern (name:id srcloc-pat value-pat)]))

;; This tries to make a port from the input strings to make more useful error
;; reporting.  Maybe this is overkill...
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

  @test-lex["traditional comment [3.7]"]{
    /* this comment /* // /** ends here: */
  }
  @test-lex["traditional comment [3.7] - ids"
            (id _ 'a) (id _ 'b)]{
    a /* this is a comment here */ b
  }

  @test-lex["integer literals [3.10.1] - decimal"
            (literal _ 1234) (literal _ 123456)
            (literal _ 123) (literal _ 123)
            (literal _ 123456) (literal _ 123456)
            (literal _ -123) (literal _ -1234)]{
    1234 123_456 123l 123L 123_456l 123_456L -123 -1234L
  }

  @test-lex["integer literals [3.10.1] - simple hexadecimal"
            (literal _ 255) (literal _ -255) (literal _ 255) (literal _ 255)
            (literal _ 170)]{
    0xfF -0xFf 0xfFl 0xFfL 0xAa
  }
  @test-lex["integer literals [3.10.1] - more hexadecimal"
            (literal _ 3671771902) (literal _ 3671771902) 
            (literal _ 3671771902) (literal _ 3671771902) 
            (literal _ 51966) (literal _ 51966) 
            (literal _ 3671771902) (literal _ -3671771902)]{
    0xDada_Cafe 0XDada_Cafe 0xdada_cafe 0Xdada_cafe 0xcafeL 0xcafel 0xdadacafe -0xdada_cafe
  }

  @test-lex["integer literals [3.10.1] - octal"
            (literal _ 10) (literal _ -10)]{
    0o12 -0o12
  }

  @test-lex["integer literals [3.10.1] - octal"
            (literal _ 2) (literal _ -3)]{
    0b10 -0b11
  }

  @test-lex["character literals [3.10.4]"
            (literal _ #\a) (literal _ #\3)
            (literal _ #\newline) (literal _ #\newline)]{
    'a' '3' '\n' '\u000a'
  }

  @test-lex["string literals [3.10.5]"
            (literal _ "abcdefg") (literal _ "") (literal _ "\"")
            (literal _ "\n\n") (literal _ "\n")]{
    "abcdefg" "" "\"" "\n\n" "\u000a"
  }

  @test-lex["separators [3.11]"
            (separator _ ";") (separator _ ",")]{
    ; , 
  }
  
  @test-lex["operators [3.12]"
            (operator _ '|.|) (operator _ '..) (operator _ '=) (operator _ '>)
            (operator _ '<) (operator _ '!) (operator _ '~) (operator _ '?)
            (operator _ ':) (operator _ '->) (operator _ '==) (operator _ '>=)
            (operator _ '<=) (operator _ '!=) (operator _ '&&) (operator _ '\|\|)
            (operator _ '++) (operator _ '--) (operator _ '+) (operator _ '-)
            (operator _ '*) (operator _ '/) (operator _ '&) (operator _ '\|)
            (operator _ '^) (operator _ '%) (operator _ '<<) (operator _ '>>)
            (operator _ '+=) (operator _ '-=) (operator _ '*=) (operator _ '/=)
            (operator _ '&=) (operator _ '\|=) (operator _ '^=) (operator _ '%=)]{
    . .. =   >   <   !   ~   ?   :   ->
    ==  >=  <=  !=  &&  ||  ++  --
    +   -   *   /   &   |   ^   %   <<   >>
    +=  -=  *=  /=  &=  |=  ^=  %=
  }
)

#lang racket/base

(require c-expr/private/lexer
         racket/match
         racket/syntax-srcloc
         syntax/srcloc)

(define (shape-name s)
  (match s
    ["(" 'parens]))

(define (shape-closer shp)
  (match shp
    ['parens ")"]))

(define (stx-cons v-srcloc v vs)
  (unless (syntax? vs)
    (error 'stx-cons "expect syntax? as second arg got: ~s" vs))
  (datum->syntax #f (cons v vs) (build-source-location v-srcloc (syntax-srcloc vs))))

(define stx-null (datum->syntax #f null #f))

(define (read-group inp)
  (define (lex) (lex-token inp))
  (define (peek) (peek-token inp))
  (define (read-literal)
    (define tok (lex))
    (stx-cons (token-srcloc tok) (token-value tok) (read-term)))
  (define (read-id)
    (define tok (lex))
    (stx-cons (token-srcloc tok) (token-value tok) (read-term)))
  (define (read-operator)
    (define tok (lex))
    (stx-cons (token-srcloc tok)
              (list 'op (token-value tok))
              (read-term)))
  (define (read-compound)
    (define tok (lex))
    (define shape (shape-name (token-value tok)))
    (define (match-closer? tok)
      (match tok
        [(closer _ (== (shape-closer shape))) #t]
        [_ #f]))
    (define (read-subgroups)
      (define ptok (peek))
      (cond
        [(match-closer? ptok) (lex) stx-null]
        ;; XXX fix exception
        [(closer? ptok) (error 'read-group "wrong closer: ~a" ptok)]
        [else
          (define g (read-group inp))
          (define g* (read-subgroups))
          (stx-cons (syntax-srcloc g) g g*)]))
    (define sub (read-subgroups))
    (stx-cons #f (stx-cons #f shape sub) (read-term)))
  (define (read-term)
    (define tok (peek))
    (cond
      [(eof-object? tok) stx-null]
      [(separator? tok) (lex) stx-null]
      [(literal? tok)   (read-literal)]
      [(id? tok)        (read-id)]
      [(operator? tok)  (read-operator)]
      [(opener? tok)    (read-compound)]
      ;; XXX check if inside a compound
      [(closer? tok)    stx-null]
      [else
        (error 'read-group "unknown token: ~a" tok)]))
  (stx-cons #f 'group (read-term)))

(module+ test
  (require racket/port
           rackunit
           syntax/parse)
  
  (let ([r (call-with-input-string "abc 123" read-group)])
    (with-check-info (['read r])
      (check-true
        (syntax-parse r
          [({~datum group} {~datum abc} 123) #t]
          [_ #f]))))

  (let ([r (call-with-input-string "123 + 456" read-group)])
    (with-check-info (['read r])
      (check-true
        (syntax-parse r
          [({~datum group} 123 ({~datum op} {~datum +}) 456) #t]
          [_ #f]))))

  (let ([r (call-with-input-string "123, 456" read-group)])
    (with-check-info (['read r])
      (check-true
        (syntax-parse r
          [({~datum group} 123) #t]
          [_ #f]))))

  (let ([r (call-with-input-string "123; 456" read-group)])
    (with-check-info (['read r])
      (check-true
        (syntax-parse r
          [({~datum group} 123) #t]
          [_ #f]))))

  (let ([r (call-with-input-string "factorial(10)" read-group)])
    (with-check-info (['read r])
      (check-true
        (syntax-parse r
          [({~datum group} {~datum factorial} ({~datum parens} ({~datum group} 10))) #t]
          [_ #f]))))

  (let ([r (call-with-input-string "printf(\"%d %s\", 42, a_string);" read-group)])
    (with-check-info (['read r])
      (check-true
        (syntax-parse r
          [({~datum group} {~datum printf}
             ({~datum parens} ({~datum group} "%d %s")
                              ({~datum group} 42)
                              ({~datum group} {~datum a_string}))) #t]
          [_ #f]))))
)

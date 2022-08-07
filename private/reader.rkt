#lang racket/base

(require c-expr/private/lexer
         racket/match
         racket/syntax-srcloc
         syntax/srcloc)

(require racket/contract
         syntax/stx)

(define/contract (stx-cons v vs)
  (-> syntax? stx-list? syntax?)
  (datum->syntax #f (cons v vs) (build-source-location v (syntax-srcloc vs))))

(define stx-null (datum->syntax #f null #f))

(define (read-literal inp)
  (define tok (lex-token inp))
  (datum->syntax #f (token-value tok) (token-srcloc tok)))

(define (read-id inp)
  (define tok (lex-token inp))
  (datum->syntax #f (token-value tok) (token-srcloc tok)))

(define (read-operator inp)
  (define tok (lex-token inp))
  (datum->syntax #f (list 'op (token-value tok)) (token-srcloc tok)))

(define (read-compound inp)
  (define tok (lex-token inp))
  (define shape (token-value tok))
  (define (match-closer? tok)
    (match tok
      [(closer _ (== shape)) #t]
      [_ #f]))
  (define (read-subgroups)
    (define ptok (peek-token inp))
    (cond
      [(match-closer? ptok) (lex-token inp) stx-null]
      ;; XXX fix exception
      [(closer? ptok) (error 'read-group "wrong closer: ~a" ptok)]
      [else
        (define g  (read-group inp))
        (define g* (read-subgroups))
        (stx-cons g g*)]))
  (define sub (read-subgroups))
  (datum->syntax #f (cons shape sub) (syntax-srcloc sub)))

(define/contract (read-term* inp)
  (-> input-port? stx-list?)
  (define-syntax-rule (read-term+ rd) (stx-cons (rd inp) (read-term* inp)))
  (define tok (peek-token inp))
  (cond
    [(eof-object? tok) stx-null]
    [(separator? tok) (lex-token inp) stx-null]
    [(literal? tok)   (read-term+ read-literal)]
    [(id? tok)        (read-term+ read-id)]
    [(operator? tok)  (read-term+ read-operator)]
    [(opener? tok)    (read-term+ read-compound)]
    [(closer? tok)    stx-null]
    [else
      ;; XXX exn type
      ;; XXX the tok from peek-tok could be #f which means the lexer has an
      ;;     error the program should expose
      (error 'read-group "unknown token: ~a" tok)]))

(define (read-group inp)
  (define g* (read-term* inp))
  (datum->syntax #f (cons 'group g*) g*))

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

  (check-exn exn:fail? (λ () (call-with-input-string ")" read-group)))
)

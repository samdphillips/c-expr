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

(define (wrong-closer-error tok)
  ;; XXX fix exception
  (error 'read-group "wrong closer: ~a" tok))

(define (unexpected-eof-error)
  ;; XXX fix exception
  (error 'read-group "unexpected eof"))

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
  (define (matching-closer? tok)
    (match tok
      [(closer _ (== shape)) #t]
      [_ #f]))
  (define sub (read-group* inp matching-closer?))
  (datum->syntax #f (cons shape sub) (syntax-srcloc sub)))

(define (read-top inp)
  (define sub (read-group* inp eof-object?))
  (datum->syntax #f (cons 'top sub) (syntax-srcloc sub)))

(define (read-group* inp group-closer?)
  (define tok (peek-token inp))
  (cond
    [(group-closer? tok) (lex-token inp) stx-null]
    [(closer? tok) (wrong-closer-error tok)]
    [else
      (define g  (read-group inp group-closer?))
      (define g* (read-group* inp group-closer?))
      (stx-cons g g*)]))

(define/contract (read-group inp group-closer?)
  (-> input-port? (-> (or/c eof-object? token?) boolean?) syntax?)
  (define g* (read-term* inp group-closer?))
  (datum->syntax #f (cons 'group g*) g*))

(define/contract (read-term* inp group-closer?)
  (-> input-port? (-> (or/c eof-object? token?) boolean?) stx-list?)
  (define-syntax-rule (read-term+ rd) (stx-cons (rd inp) (read-term* inp group-closer?)))
  (define tok (peek-token inp))
  (cond
    [(group-closer? tok) stx-null]
    [(separator? tok)    (lex-token inp) stx-null]
    [(literal? tok)      (read-term+ read-literal)]
    [(id? tok)           (read-term+ read-id)]
    [(operator? tok)     (read-term+ read-operator)]
    [(opener? tok)       (read-term+ read-compound)]
    [(eof-object? tok)   (unexpected-eof-error)]
    [(closer? tok)       (wrong-closer-error)]
    ;; if tok is #f the lexer has an error, this will propagate it out
    [(not tok)           (lex-token inp)]
    [else
      ;; XXX exn type
      (error 'read-group "unknown token: ~a" tok)]))

(module+ test
  (require racket/port
           rackunit
           syntax/parse)
  
  (define-literal-set c-expr-litset
    #:datum-literals (top group op parens) ())
  
  (test-begin
    (let ([r (call-with-input-string "abc 123" read-top)])
      (with-check-info (['read r])
        (check-true
          (syntax-parse r
            #:literal-sets (c-expr-litset)
            [(top (group {~datum abc} 123)) #t]
            [_ #f])))))

  (test-begin
    (let ([r (call-with-input-string "123 + 456" read-top)])
      (with-check-info (['read r])
        (check-true
          (syntax-parse r
            #:literal-sets (c-expr-litset)
            [(top (group 123 (op {~datum +}) 456)) #t]
            [_ #f])))))

  (test-begin
    (let ([r (call-with-input-string "123, 456" read-top)])
      (with-check-info (['read r])
        (check-true
          (syntax-parse r
            #:literal-sets (c-expr-litset)
            [(top (group 123) (group 456)) #t]
            [_ #f])))))

  (test-begin
    (let ([r (call-with-input-string "123; 456" read-top)])
      (with-check-info (['read r])
        (check-true
          (syntax-parse r
            #:literal-sets (c-expr-litset)
            [(top (group 123) (group 456)) #t]
            [_ #f])))))

  (test-begin
    (let ([r (call-with-input-string "factorial(10)" read-top)])
      (with-check-info (['read r])
        (check-true
          (syntax-parse r
            #:literal-sets (c-expr-litset)
            [(top (group {~datum factorial} (parens (group 10)))) #t]
            [_ #f])))))

  (test-begin
    (let ([r (call-with-input-string "printf(\"%d %s\", 42, a_string);" read-top)])
      (with-check-info (['read r])
        (check-true
          (syntax-parse r
            #:literal-sets (c-expr-litset)
            [(top (group {~datum printf}
                         (parens (group "%d %s")
                                 (group 42)
                                 (group {~datum a_string})))) #t]
            [_ #f])))))

  (test-begin (check-exn exn:fail? (λ () (call-with-input-string ")" read-top))))
)

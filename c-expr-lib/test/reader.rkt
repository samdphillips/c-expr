#lang racket/base

(require c-expr/private/reader)

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

  (test-begin (check-exn exn:fail? (λ () (call-with-input-string ")" read-top)))))

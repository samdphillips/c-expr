#lang racket/base

(require (for-syntax enforest
                     enforest/operator
                     enforest/transformer
                     racket/base
                     #;racket/format
                     syntax/parse)
         syntax/parse/define)

(provide (rename-out [module-begin #%module-begin])
         fn)

(define-syntax module-begin
  (syntax-parser
    #:datum-literals (top)
    [(_ (top . body)) #'(#%module-begin (example-top . body))]))

(begin-for-syntax
  (struct definition-transformer (proc))

  (define (definition-transformer-ref v)
    (and (definition-transformer? v)
         (definition-transformer-proc v)))

  (define-syntax-class :definition
    #:datum-literals (group)
    (pattern
     (group def:id . t)
     #:do [(define proc
             (definition-transformer-ref
               (syntax-local-value #'def (λ () #f))))]
     #:fail-unless proc "definition"
     #:do [(define-values (head-stx tail-stx) (proc #'(def . t)))]
     #:attr parsed #`(begin #,head-stx (example-begin  #,@tail-stx))))

  (define-syntax-class :formals
    #:datum-literals (parens group)
    #:attributes (parsed)
    (pattern
     (parens (group name:id) ...)
     #:attr parsed #'(name ...)))

  (define (prefix-operator-ref v)
    (and (prefix-operator? v) v))

  (define (infix-operator-ref v)
    (and (infix-operator? v) v))

  (define-enforest
    #:syntax-class :expression
    #:prefix-operator-ref prefix-operator-ref
    #:infix-operator-ref infix-operator-ref
    )

)

(define-syntax fn
  (definition-transformer
    (syntax-parser
      #:datum-literals (braces)
      [(_ name:id args::formals (braces body ...) . tail)
       (values #'(define (name . args.parsed)
                   (example-block body ...))
               #'tail)])))

(define-syntax (example-top stx)
  (syntax-parse stx
    [(_) #'(begin)]
    [(_ form . forms)
     #:with parsed
     (syntax-parse #'form
       [e::definition #'(begin . e.parsed)]
       [e::expression #'(#%expression e.parsed)])
     #'(begin parsed (example-top . forms))]))

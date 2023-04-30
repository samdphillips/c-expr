#lang racket/base

(require (for-syntax racket/base
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
      #:attr parsed #`(begin #,head-stx)
      #:attr tail tail-stx))

  (define-syntax-class :formals
    #:datum-literals (parens group)
    #:attributes (parsed)
    (pattern
      (parens (group name:id) ...)
      #:attr parsed #'(name ...)))
)

(define-syntax fn
  (definition-transformer
    (syntax-parser
      #:datum-literals (braces)
      [(_ name:id args::formals (braces body ...) . tail)
       (values #'(define (name . args.parsed)
                   (example-block body ...))
               #'tail)])))

(define-syntax-parser example-block
  [(_ e::expression)
   #'(#%expression e.parsed)]
  [(_ form . forms)
   #:with parsed
   (syntax-parse #'form
     [e::definition #'(begin e.parsed (example-begin (group . e.tail)))]
     [e::expression #'(#%expression e.parsed)])
   #'(let () parsed (example-begin . forms))])

(define-syntax (example-top stx)
  (syntax-parse stx
    [(_) #'(begin)]
    [(_ form . forms)
     #:with parsed
     (syntax-parse #'form
       #:datum-literals (group)
       [(group) #'(begin)]
       [e::definition
        #'(begin e.parsed (example-top (group . e.tail)))]
       [e::expression #'(#%expression e.parsed)])
     #'(begin parsed (example-top . forms))]))

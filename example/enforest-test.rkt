#lang racket

(require syntax/parse)

(struct operator (name combiner stronger weaker) #:transparent)

(define op-table
  (hash '+ (operator '+
                     (λ (lhs rhs) #`(+ #,lhs #,rhs))
                     '(-)
                     '(+ * /))
        '* (operator '*
                     (λ (lhs rhs) #`(* #,lhs #,rhs))
                     null
                     '(*))))

(define-syntax-class :operator
  [pattern ({~datum op} name:id)
    #:do [(define v (hash-ref op-table (syntax->datum #'name) #f))]
    #:when (operator? v)
    #:attr value v])

(define (stronger-than? a b)
  (define aname (and a (operator-name a)))
  (define bname (and b (operator-name b)))
  (cond
    [(not aname) #f]
    [(not bname) #t]
    [(member aname (operator-weaker b)) #t]
    [(member bname (operator-stronger a)) #t]
    [else #f]))

(define (weaker-than? a b) (stronger-than? b a))

(define (operator-combine op l r)
  ((operator-combiner op) l r))

(define enforest1
  (case-lambda
    [(stx op)
     (syntax-parse stx
       [() (values stx stx)]
       [(({~datum parens} . h) . t)
        (define-values (hh ht) (enforest1 #'h #f))
        (syntax-parse ht
          [() (enforest1 hh #'t #f)])]
       [(x . rest)
        (syntax-parse #'x
          [x (enforest1 #'x #'rest op)])])]
    [(h t current-op)
     (syntax-parse t
       [() (values h t)]
       [(op::operator . post)
        (define new-op (attribute op.value))
        (cond
          [(stronger-than? new-op current-op)
           (define-values (rh rest) (enforest1 #'post (attribute op.value)))
           (values (operator-combine new-op h rh) rest)]
          [(weaker-than? new-op current-op)
           (values h t)]
          [else
           (error 'enforest1 "unable to determine precedence: ~a ~a"
                  (operator-name current-op)
                  (operator-name new-op))])])]))

(define (enforest stx)
  (define-values (h t) (enforest1 stx #f))
  (syntax-parse t
    [() h]
    [_ (enforest #`(#,h . #,t))]))

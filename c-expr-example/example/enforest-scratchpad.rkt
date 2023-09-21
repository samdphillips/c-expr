#lang racket/base

(require (for-syntax enforest
                     enforest/operator
                     racket/base
                     syntax/parse)
         (prefix-in r: racket/base))

(begin-for-syntax
  (define-enforest
    #:syntax-class :expression
    #:prefix-operator-ref values
    #:infix-operator-ref  values))

(define-syntax #%literal
  (prefix-operator
   #'#%literal
   null
   'macro
   (syntax-parser
     [(_ v . tail) (values #'v #'tail)])))

(define-syntax #%parens
  (prefix-operator
   #'#%parens
   null
   'macro
   (syntax-parser
     #:datum-literals (parens)
     [(_ (parens e::expression) . tail)
      (values #'e.parsed #'tail)])))

(define-syntax #%call
  (infix-operator
   #'#%call
   null
   'macro
   (λ (rator rh)
     (displayln rh)
     (syntax-parse rh
       #:datum-literals (parens group)
       [(_ (parens rands::expression ...) . tail)
        (values #`(#,rator rands.parsed ...) #'tail)]))
   'none))

(define-syntax #%juxtapose
  (infix-operator
   #'#%juxtapose
   null
   'automatic
   (λ (lh rh op)
     (displayln (list 'juxtapose lh rh op))
     #`(begin #,lh #,rh))
   'none))

(define-syntax if
  (prefix-operator
   #'if
   null
   'macro
   (syntax-parser
     #:datum-literals (else parens braces)
     [(_ (parens tst::expression)
         (braces conseq-body::expression ...)
         else
         (braces alt-body::expression ...)
         .
         tail)
      (values #'(r:if tst.parsed
                      (begin conseq-body.parsed ...)
                      (begin alt-body.parsed ...))
              #'tail)])))

(define-syntax +
  (infix-operator
   #'+
   null
   'automatic
   (λ (lh rh op)
     (quasisyntax/loc op (r:+ #,lh #,rh)))
   'left))

(define-syntax ==
  (infix-operator
   #'==
   null
   'automatic
   (λ (lh rh op)
     (quasisyntax/loc op (= #,lh #,rh)))
   'left))
  
(define-syntax do-enforest
  (syntax-parser
    [(_ e::expression) #'e.parsed]))


(define a 32)
(define b 32)


(do-enforest
 (group
  if
  (parens (group a (op ==) b))
  (braces (group a))
  else
  (braces
   (group b))
  10))

#;
(do-enforest (group (parens (group 10 (op +) a))))

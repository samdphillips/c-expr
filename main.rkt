#lang racket/base

#|
    https://docs.oracle.com/javase/specs/jls/se8/html/jls-3.html
    Differences:
    - [3.3] [P] Unicode escapes are not converted before lexing.
    - [3.8] [B] I am using the default alphabetic character class for identifiers, and
      have not ensured that all unicode "letters" are allowed.  If you find a
      unicode value that is not accepted as an identifier and it passes the
      criteria for "JavaLetter" it is a bug in this lexer.
    - [3.9] [X] There are no reserved keywords at the lexer level.
    - [3.10] [X] There are no boolean or null literals at the lexer level.
    - [3.10.1] [?] Integer type suffixes are ignored
    - [3.10.1] [P] Octal literals start with '0o' or '0O'
|#

(require (prefix-in - syntax/readerr)
         (prefix-in : parser-tools/lex-sre)
         parser-tools/lex
         racket/match
         syntax/srcloc)

(provide lex-c-expr)

(define (->srcloc source pos)
  (and pos
       (match-let ([(position offset line column) pos])
         (vector (if (input-port? source)
                     (object-name source)
                     source)
                 line column offset 0))))

(define ((make-raise-read-error raise-proc) message srcloc)
  (raise-proc message
              (source-location-source   srcloc)
              (source-location-line     srcloc)
              (source-location-column   srcloc)
              (source-location-position srcloc)
              (source-location-span     srcloc)))

(define raise-read-error
  (make-raise-read-error -raise-read-error))
(define raise-read-eof-error
  (make-raise-read-error -raise-read-eof-error))

(struct token (srcloc value) #:transparent)
(define-syntax-rule (define-token name)
  (begin (struct name token () #:transparent)
         (provide (struct-out name))))
(define-token id)
(define-token literal)
(define-token opener)
(define-token closer)

(define lex-c-expr
  (let ()
    (define-syntax-rule ($token maker value)
      (maker (build-source-location-vector
               (->srcloc input-port start-pos)
               (->srcloc input-port end-pos))
             value))
    (lexer
      [(eof) eof]
      ;; whitespace
      [(:+ whitespace) (lex-c-expr input-port)]
      ;; comments
      [(:: "//" (:* (:~ #\newline))) (lex-c-expr input-port)]
      [(:: "/*") (skip-trad-comment input-port)]

      ;; identifiers
      [(:: (:or #\$ #\_ alphabetic)
           (:* (:or #\$ #\_ alphabetic numeric)))
       ($token id (string->symbol lexeme))]

      ;; [3.10.1] Integer literals
      [(:: (:? #\-) numeric (:* (:or #\_ numeric)) (:? (:or #\L #\l)))
       (let ([neg? (if (char=? #\- (string-ref lexeme 0)) #t #f)])
         ($token literal (integer-literal->integer lexeme neg? (if neg? 1 0) 10)))]

      ;; openers
      [(:or #\( #\{ #\[)
       ($token opener lexeme)]

      ;; closers
      [(:or #\) #\} #\])
       ($token closer lexeme)]

      ;; error
      [any-char (raise-read-error
                 (format "No match found for input starting with: ~s" lexeme)
                 (->srcloc input-port start-pos))])))

(define skip-trad-comment
  (let ()
    (define (eof-error) (raise-read-eof-error "eof in comment"))
    (define comment-tail
      (lexer
        [(eof) (eof-error)]
        [#\*   (comment-tail-star input-port)]
        [any-char (comment-tail input-port)]))
    (define comment-tail-star
      (lexer
        [(eof) (eof-error)]
        [#\/ (lex-c-expr input-port)]
        [#\* (comment-tail-star input-port)]
        [any-char (comment-tail input-port)]))
    comment-tail))

(define ch0 (char->integer #\0))
(define (cdelta ch v) (- (char->integer ch) v))

(define (integer-literal->integer str neg? skip base)
  (define char-value
    (match base
     [10 (lambda (c) (cdelta c ch0))]))
  (for/fold ([v 0] #:result (if neg? (- v) v)) 
            ([c (in-string str skip)]
                     #:unless (or (char=? c #\_) 
                                  (char=? c #\L)
                                  (char=? c #\l)))
    (+ (* base v) (char-value c))))

#lang racket/unit

#|
    https://docs.oracle.com/javase/specs/jls/se8/html/jls-3.html
    Differences:
    - [3.3] [P] Unicode escapes are not converted before lexing.
    - [3.8] [B] I am using the default alphabetic character class for identifiers, and
      have not ensured that all unicode "letters" are allowed.  If you find a
      unicode value that is not accepted as an identifier and it passes the
      criteria for "JavaLetter" it is a bug in this lexer.
    - [3.9] [X] There are no reserved keywords at the lexer level.
    - [3.10.1] [P] Octal literals start with '0o' or '0O'
    - [3.10.1] [?] Integer type suffixes are ignored
    - [3.10.1] [X] Literals integers do not have a fixed range.
    - [3.10.1] [X] Hexadecimal, octal, and binary integer literals are not in
      twos-complement encoding.
    - [3.10.1] [X] Hexadecimal, octal, and binary integer literals are allowed
      to have a leading `-` for negative values.
    - [3.10.2] [P] Floating point literals are not implemented.
    - [3.10.3] [X] There are no boolean literals at the lexer level.
    - [3.10.5] [X] String literals are allowed to contain line terminators.
    - [3.10.6] [P] Octal escapes are not supported in strings or character literals.
    - [3.10.6] [X] Unicode escapes are handled in string and character literals.
    - [3.10.6] [P] UTF-16 surrogate pairs are not supported.
    - [3.10.7] [X] There is no null literal at the lexer level.
    - [3.11] [P] Separators ... @ :: are not implemented
    - [3.11] [X] Separator . is treated as an operator
    - [3.12] [X] Operators are any string of operator characters.

    TODO:
    - add \UXXXXXXXX to encode code points greater than 16 bits
    - escape to Racket reader ala Shrubbery notation #{}
|#

(require (prefix-in : parser-tools/lex-sre)
         (except-in parser-tools/lex token? token-value)
         racket/match
         racket/port
         syntax/srcloc
         "exn.rkt"
         "lexer-sig.rkt"
         "lexer-ext-sig.rkt"
         "token.rkt")

(import lexer-ext^)
(export lexer^)

(define-lex-abbrev hexdigit (:or numeric (:/ "AF" "af")))
(define-lex-abbrev octdigit (:/ "07"))

(define lex-token
  (let ()
    (define-syntax-rule ($token maker value)
      (maker (build-source-location-vector
               (->srcloc input-port start-pos)
               (->srcloc input-port end-pos))
             value))
    (lexer
      [(eof) eof]
      ;; whitespace
      [(:+ whitespace) (lex-token input-port)]
      ;; comments
      [(:: "//" (:* (:~ #\newline))) (lex-token input-port)]
      [(:: "/*" (:* #\*)) (skip-trad-comment input-port)]

      ;; identifiers
      [(:: (:or #\$ #\_ alphabetic)
           (:* (:or #\$ #\_ alphabetic numeric)))
       ($token id (string->symbol lexeme))]

      ;; [3.10.1] Integer literals
      ;;   decimal integers
      [(:: (:? #\-) numeric (:* (:or #\_ numeric)) (:? (:or #\L #\l)))
       (let ([neg? (if (char=? #\- (string-ref lexeme 0)) #t #f)])
         ($token literal (integer-literal->integer lexeme neg? (if neg? 1 0) 10)))]
      ;;   hexadecimal integers
      [(:: (:? #\-) #\0 (:or #\x #\X) (:+ (:or #\_ hexdigit)) (:? (:or #\L #\l)))
       (let ([neg? (if (char=? #\- (string-ref lexeme 0)) #t #f)])
         ($token literal (integer-literal->integer lexeme neg? (if neg? 3 2) 16)))]
      ;;   octal integers
      [(:: (:? #\-) #\0 (:or #\o #\O) (:+ (:or #\_ octdigit)) (:? (:or #\L #\l)))
       (let ([neg? (if (char=? #\- (string-ref lexeme 0)) #t #f)])
         ($token literal (integer-literal->integer lexeme neg? (if neg? 3 2) 8)))]
      ;;   binary integers
      [(:: (:? #\-) #\0 (:or #\b #\B) (:+ (:or #\_ #\0 #\1)) (:? (:or #\L #\l)))
       (let ([neg? (if (char=? #\- (string-ref lexeme 0)) #t #f)])
         ($token literal (integer-literal->integer lexeme neg? (if neg? 3 2) 2)))]

      ;; [3.10.4] Character literals
      ;;     unescaped
      [(:: #\' (:~ #\\ #\' #\newline #\return) #\')
       ($token literal (string-ref lexeme 1))]
      ;;     char escapes
      [(:: #\' #\\ (char-set "btnfr\"'\\") #\')
       ($token literal (escape-ref (string-ref lexeme 2)))]
      ;;     unicode escapes
      [(:: #\' #\\ #\u (:= 4 hexdigit) #\')
       ($token literal (integer->char (string->number (substring lexeme 3 7) 16)))]

      ;; [3.10.5] String Literals
      [(:: #\") (lex-string start-pos input-port)]

      ;; [3.11] Separators
      ;;     openers
      [(:or #\( #\{ #\[) ($token opener (separator-name lexeme))]
      ;;     closers
      [(:or #\) #\} #\]) ($token closer (separator-name lexeme))]
      ;;     other punctuators
      [#\; ($token major-separator lexeme)]
      [#\, ($token minor-separator lexeme)]

      ;; [3.12] Operators
      [(:or (:** 1 2 #\.) (:+ (char-set "<>?/:!%^&*-+=|~")))
       ($token operator (string->symbol lexeme))]

      ;; error
      [any-char (handle-unknown-lexeme lexeme start-pos input-port)])))

(define (peek-token inp)
  (define peek-inp (peeking-input-port inp))
  (dynamic-wind
    void
    (lambda ()
      (with-handlers* ([exn:fail:read? (lambda (e) #f)])
        (lex-token peek-inp)))
    (lambda () (close-input-port peek-inp))))

(define (separator-name s)
  (match s
   [(or "(" ")") 'parens]
   [(or "[" "]") 'brackets]
   [(or "{" "}") 'braces]))

(define skip-trad-comment
  (let ()
    (define (eof-error srcloc) (raise-read-eof-error "eof in comment" srcloc))
    (define comment-tail
      (lexer
        [(eof) (eof-error (->srcloc input-port start-pos))]
        [#\*   (comment-tail-star input-port)]
        [any-char (comment-tail input-port)]))
    (define comment-tail-star
      (lexer
        [(eof) (eof-error (->srcloc input-port start-pos))]
        [#\/ (lex-token input-port)]
        [#\* (comment-tail-star input-port)]
        [any-char (comment-tail input-port)]))
    comment-tail))

(define ch0 (char->integer #\0))
(define chA (char->integer #\A))
(define cha (char->integer #\a))

(define (integer-literal->integer str neg? skip base)
  (define char-value
    (match base
     [(or 2 8 10)
      (lambda (c) (- (char->integer c) ch0))]
     [16 (lambda (c)
           (let ([c (char->integer c)])
             (cond
              [(>= c cha) (+ 10 (- c cha))]
              [(>= c chA) (+ 10 (- c chA))]
              [else (- c ch0)])))]))
  (for/fold ([v 0] #:result (if neg? (- v) v))
            ([c (in-string str skip)]
             #:unless (or (char=? c #\_)
                          (char=? c #\L)
                          (char=? c #\l)))
    (+ (* base v) (char-value c))))

(define (lex-string start-pos input-port)
  (define buf (open-output-string))
  (define (buffer-add! s)
    (write-string s buf)
    (lex input-port))
  (define (buffer-finalize end-pos)
    (close-output-port buf)
    (literal (build-source-location-vector
               (->srcloc input-port start-pos)
               (->srcloc input-port end-pos))
             (get-output-string buf)))
  (define lex
    (lexer
      [(eof) (raise-read-eof-error "eof in string" (->srcloc input-port start-pos))]
      [(:+ (:~ #\\ #\")) (buffer-add! lexeme)]
      [(:: #\\ (char-set "btnfr\"'\\"))
       (buffer-add! (string (escape-ref (string-ref lexeme 1))))]
      [(:: #\\ #\u (:= 4 hexdigit))
       (buffer-add! (string (integer->char (string->number (substring lexeme 2) 16))))]
      [(:: #\\ any-char)
       (raise-read-error
         (format "unknown string escape: ~a" lexeme)
         (build-source-location-vector
           (->srcloc input-port start-pos)
           (->srcloc input-port end-pos)))]
      [#\" (buffer-finalize end-pos)]
      ))
  (lex input-port))

(define escape-ref
  (let ([escapes
           (hash #\b #\backspace
                 #\t #\tab
                 #\n #\newline
                 #\f #\page
                 #\r #\return)])
    (lambda (ch)
      (hash-ref escapes ch ch))))

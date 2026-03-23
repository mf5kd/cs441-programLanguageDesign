#lang racket

;; The regex splits the file into processable chunks
(define (split-string str)
  (regexp-match* #px"\n|[a-zA-Z_][a-zA-Z0-9_\\-]*|\\d+\\.\\d+|\\d+|/\\*|\\*/|:=|!=|>=|<=|[;()+\\-*/=><]|\\S+" str))

(define input-file "input/TestInput.txt")

(define (read-file input-file) ; -> string
  (file->string input-file))

;; Helper function to safely skip block comments
(define (skip-comments lst)
  (cond
    [(empty? lst) '()] ; Safety check for unclosed comments
    [(equal? (first lst) "*/") (rest lst)]
    [else (skip-comments (rest lst))]))

;; Main lexer function using an accumulator (tokens) for O(N) performance
(define (tokenize clean-source [tokens '()])
  (define split-clean-source
    (if (string? clean-source)
        (split-string clean-source)
        clean-source))

  (cond
    ;; Base Case: Reached the end. Reverse the accumulated list and append EOF.
    [(empty? split-clean-source)
     (reverse (cons 'EOF-TOK tokens))]
    
    ;; Recursive Step: Process the current token
    [else
     (let ([current (first split-clean-source)]
           [next (rest split-clean-source)])
       (cond
         ;; --- Comments & Whitespace ---
         [(equal? current "/*") (tokenize (skip-comments next) tokens)]
         [(equal? current "\n") (tokenize next (cons 'NL-TOK tokens))]
         
         ;; --- Operators & Punctuation ---
         [(equal? current ":=") (tokenize next (cons 'ASSIGN-TOK tokens))]
         [(equal? current "(")  (tokenize next (cons 'START-PAR-TOK tokens))]
         [(equal? current ")")  (tokenize next (cons 'END-PAR-TOK tokens))]
         [(equal? current ";")  (tokenize next (cons 'SEMI-TOK tokens))]
         [(equal? current "+")  (tokenize next (cons 'ADD-TOK tokens))]
         [(equal? current "-")  (tokenize next (cons 'SUB-TOK tokens))]
         [(equal? current "/")  (tokenize next (cons 'DIV-TOK tokens))]
         [(equal? current "*")  (tokenize next (cons 'MUL-TOK tokens))]
         [(equal? current "=")  (tokenize next (cons 'EQ-TOK tokens))]
         [(equal? current "!=") (tokenize next (cons 'NOT-EQ-TOK tokens))]
         [(equal? current ">")  (tokenize next (cons 'GT-TOK tokens))]
         [(equal? current ">=") (tokenize next (cons 'GT-EQ-TOK tokens))]
         [(equal? current "<")  (tokenize next (cons 'LT-TOK tokens))]
         [(equal? current "<=") (tokenize next (cons 'LT-EQ-TOK tokens))]
         
         ;; --- Keywords ---
         [(equal? current "PRINT") (tokenize next (cons 'PRINT-TOK tokens))]
         [(equal? current "IF")    (tokenize next (cons 'IF-TOK tokens))]
         [(equal? current "THEN")  (tokenize next (cons 'THEN-TOK tokens))]
         [(equal? current "ELSE")  (tokenize next (cons 'ELSE-TOK tokens))]
         [(equal? current "END")   (tokenize next (cons 'END-BLK-TOK tokens))]
         [(equal? current "WHILE") (tokenize next (cons 'WHILE-TOK tokens))]
         [(equal? current "DO")    (tokenize next (cons 'DO-TOK tokens))]
         
         ;; --- Identifiers & Numbers ---
         [else
          (let ([num (string->number current)])
            (cond
              ;; Is it an integer?
              [(and num (exact-integer? num)) 
               (tokenize next (cons (list 'INT-TOK num) tokens))]
              ;; Is it a floating point?
              [(and num (inexact-real? num))  
               (tokenize next (cons (list 'FP-TOK num) tokens))]
              ;; Does it match the <ID> grammar?
              [(regexp-match? #px"^[a-zA-Z_][a-zA-Z0-9_\\-]*$" current)
               (tokenize next (cons (list 'ID-TOK current) tokens))]
              ;; Ignore anything else (like raw spaces caught by the regex)
              [else
               (tokenize next tokens)]))]))]))

(define source-file (read-file input-file))
(displayln (split-string source-file))
(tokenize source-file)

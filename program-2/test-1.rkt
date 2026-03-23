#lang racket

(define (split-string str)
  (regexp-match* #px"\n|[a-zA-Z_][a-zA-Z0-9_\\-]*|\\d+\\.\\d+|\\d+|/\\*|\\*/|:=|!=|>=|<=|[;()+\\-*/=><]|\\S+" str))

(define input-file "input/Input1.txt")

(define (read-file input-file) ; -> lst
  (file->string input-file))

(define (tokenize clean-source [tokens '()] [old-item ""])
  (define split-clean-source 
    (if (string? clean-source) 
        (split-string clean-source) 
        clean-source))
  (define (strip-comments lst)
    (cond
      [(equal? "*/" (first lst)) (rest lst)]
      [else 
          (strip-comments (rest lst))]))
  (cond 
    [(empty? split-clean-source) (append tokens (list 'EOF-TOK))]
    [else 
        (let* ([before old-item]
               [current (first split-clean-source)]
               [next (rest split-clean-source)]
               [num (string->number current)])
          (cond 
            [(equal? current "/*") (tokenize (strip-comments split-clean-source) tokens current)]
            [(equal? current "\n") (tokenize next (append tokens (list 'NL-TOK)) current)]
            [(equal? current ":=") (tokenize next (append tokens (list (list 'ID-TOK before) 'ASSIGN-TOK)) current)]
            [(equal? current "(")  (tokenize next (append tokens (list 'START-PAR-TOK)) current)]
            [(equal? current ")")  (tokenize next (append tokens (list 'END-PAR-TOK)) current)]
            [(equal? current ";")  (tokenize next (append tokens (list 'SEMI-TOK)) current)]
            [(equal? current "+") (tokenize next (append tokens (list 'ADD-TOK)) current)]
            [(equal? current "-") (tokenize next (append tokens (list 'SUB-TOK)) current)]
            [(equal? current "/") (tokenize next (append tokens (list 'DIV-TOK)) current)]
            [(equal? current "*") (tokenize next (append tokens (list 'MUL-TOK)) current)]
            [(equal? current "=") (tokenize next (append tokens (list 'EQ-TOK)) current)]
            [(equal? current "!=") (tokenize next (append tokens (list 'NOT-EQ-TOK)) current)]
            [(equal? current ">") (tokenize next (append tokens (list 'GT-TOK)) current)]
            [(equal? current ">=") (tokenize next (append tokens (list 'GT-EQ-TOK)) current)]
            [(equal? current "<") (tokenize next (append tokens (list 'LT-TOK)) current)]
            [(equal? current "<=") (tokenize next (append tokens (list 'LT-EQ-TOK)) current)]
            [(equal? current "PRINT") (tokenize next (append tokens (list 'PRINT-TOK)) current)]
            [(equal? current "IF") (tokenize next (append tokens (list 'IF-TOK)) current)]
            [(equal? current "THEN") (tokenize next (append tokens (list 'THEN-TOK)) current)]
            [(equal? current "ELSE") (tokenize next (append tokens (list 'ELSE-TOK)) current)]
            [(equal? current "END") (tokenize next (append tokens (list 'END-BLK-TOK)) current)]
            [(equal? current "WHILE") (tokenize next (append tokens (list 'WHILE-TOK)) current)]
            [(equal? current "DO") (tokenize next (append tokens (list 'DO-TOK)) current)]
            [num (cond
                   [(integer? num) (tokenize next (append tokens (list (list 'INT-TOK num))) current)]
                   [(flonum? num)  (tokenize next (append tokens (list (list 'FP-TOK num))) current)])]    
            [else 
                (tokenize next tokens current)]))]))

;(tokenize (rest clean-source))

; --------------------------------------------------------------------------------------------------------------
(define source-file (read-file input-file))

(displayln (split-string source-file))

(tokenize source-file)
(newline)

(string->number "1.")



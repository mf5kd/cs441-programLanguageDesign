#lang racket

;; The regex splits the file into processable chunks
(define (split-string str)
  (regexp-match* #px"\n|[a-zA-Z_][a-zA-Z0-9_\\-]*|\\d+\\.\\d+|\\d+|/\\*|\\*/|:=|!=|>=|<=|[;()+\\-*/=><]|\\S+" str))

(define input-file1 "input/Input1.txt")
(define input-file2 "input/Input2.txt")
(define input-file3 "input/Input3.txt")
(define input-file4 "input/Input4.txt")
(define input-file5 "input/Input5.txt")
(define input-file6 "input/Input6.txt")
(define input-file7 "input/Input7.txt")

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

;; --- State Management & Line Number Calculation ---

;; A functional parameter to hold the root token list for error reporting
(define current-orig-tokens (make-parameter '()))

;; Functionally calculates the current line by finding our position in the original list
(define (get-line original-tokens current-tokens)
  (let loop ([toks original-tokens] [line 1])
    (cond
      [(eq? toks current-tokens) line] ; Found our exact position via pointer equality
      [(empty? toks) line]
      [(equal? (first toks) 'NL-TOK) (loop (rest toks) (+ line 1))]
      [else (loop (rest toks) line)])))

;; Safely strips newlines for the parser so it only sees structural tokens
(define (skip-newlines tokens)
  (cond
    [(empty? tokens) '()]
    [(equal? (first tokens) 'NL-TOK) (skip-newlines (rest tokens))]
    [else tokens]))

;; --- Core Parser Helpers ---

(define (peek tokens)
  (let ([cleaned (skip-newlines tokens)])
    (if (empty? cleaned)
        'EOF-TOK
        (first cleaned))))

(define (syntax-error tokens expected-str)
  (let ([line (get-line (current-orig-tokens) tokens)])
    (raise-user-error (format "SYNTAX ERROR missing or expected ~a at line ~a" expected-str line))))

;; Returns the remaining tokens after consuming
(define (consume tokens expected-type err-msg)
  (let ([cleaned (skip-newlines tokens)])
    (if (empty? cleaned)
        (syntax-error cleaned err-msg)
        (let ([tok (first cleaned)])
          (if (if (symbol? expected-type)
                  (equal? tok expected-type)
                  (and (list? tok) (equal? (first tok) expected-type)))
              (rest cleaned)
              (syntax-error cleaned err-msg))))))

;; Extracts values from compound tokens like (INT-TOK 5) -> returns (cons Value Remaining-Tokens)
(define (consume-val tokens expected-type err-msg)
  (let ([cleaned (skip-newlines tokens)])
    (if (empty? cleaned)
        (syntax-error tokens err-msg)
        (let ([tok (first cleaned)])
          (if (and (list? tok) (equal? (first tok) expected-type))
              (cons (second tok) (rest cleaned))
              (syntax-error tokens err-msg)))))) 

;; --- The Recursive Descent Parser ---

(define (parse-program tokens)
  ;; Bind the initial token list for error reference
  (parameterize ([current-orig-tokens tokens])
    (let* ([res (parse-stmt-list tokens)]
           [ast (car res)]
           [rem (cdr res)]
           [final-rem (skip-newlines rem)])
      (if (or (empty? final-rem) (equal? (first final-rem) 'EOF-TOK))
          (cons (cons 'program ast) final-rem)
          (syntax-error final-rem "EOF")))))

(define (parse-stmt-list tokens)
  (let* ([res1 (parse-statement tokens)]
         [ast1 (car res1)]
         [rem1 (cdr res1)]
         [next-tok (peek rem1)])
    ;; Check if the statement list has reached a terminal boundary
    (if (or (equal? next-tok 'EOF-TOK)
            (equal? next-tok 'END-BLK-TOK)
            (equal? next-tok 'ELSE-TOK))
        (cons (list ast1) rem1)
        (let* ([res2 (parse-stmt-list rem1)]
               [ast2 (car res2)]
               [rem2 (cdr res2)])
          (cons (cons ast1 ast2) rem2)))))

(define (parse-statement tokens)
  (let ([next-tok (peek tokens)])
    (cond
      [(equal? next-tok 'IF-TOK) (parse-if-stmt tokens)]
      [(equal? next-tok 'WHILE-TOK) (parse-while-stmt tokens)]
      [(equal? next-tok 'PRINT-TOK) (parse-print-stmt tokens)]
      [(and (list? next-tok) (equal? (first next-tok) 'ID-TOK)) (parse-assign-stmt tokens)]
      [else (syntax-error tokens "statement (IF, WHILE, PRINT, or ID)")])))

(define (parse-assign-stmt tokens)
  (let* ([val-res (consume-val tokens 'ID-TOK "Identifier")]
         [id (string->symbol (car val-res))]
         [rem1 (consume (cdr val-res) 'ASSIGN-TOK "\":=\"")]
         [expr-res (parse-expression rem1)]
         [expr-ast (car expr-res)]
         [rem2 (consume (cdr expr-res) 'SEMI-TOK "\";\"")])
    (cons (list 'assign id expr-ast) rem2)))

(define (parse-if-stmt tokens)
  (let* ([rem1 (consume tokens 'IF-TOK "\"IF\"")]
         [comp-res (parse-comparison rem1)]
         [comp-ast (car comp-res)]
         [rem2 (consume (cdr comp-res) 'THEN-TOK "\"THEN\"")]
         [then-res (parse-stmt-list rem2)]
         [then-ast (car then-res)]
         [rem3 (consume (cdr then-res) 'ELSE-TOK "\"ELSE\"")]
         [else-res (parse-stmt-list rem3)]
         [else-ast (car else-res)]
         [rem4 (consume (cdr else-res) 'END-BLK-TOK "\"END\"")])
    (cons (list 'if comp-ast then-ast else-ast) rem4)))

(define (parse-while-stmt tokens)
  (let* ([rem1 (consume tokens 'WHILE-TOK "\"WHILE\"")]
         [comp-res (parse-comparison rem1)]
         [comp-ast (car comp-res)]
         [rem2 (consume (cdr comp-res) 'DO-TOK "\"DO\"")]
         [body-res (parse-stmt-list rem2)]
         [body-ast (car body-res)]
         [rem3 (consume (cdr body-res) 'END-BLK-TOK "\"END\"")])
    (cons (append (list 'while comp-ast) body-ast) rem3)))

(define (parse-print-stmt tokens)
  (let* ([rem1 (consume tokens 'PRINT-TOK "\"PRINT\"")]
         [expr-res (parse-expression rem1)]
         [expr-ast (car expr-res)]
         [rem2 (consume (cdr expr-res) 'SEMI-TOK "\";\"")])
    (cons (list 'print expr-ast) rem2)))

(define (parse-comparison tokens)
  (let* ([expr1-res (parse-expression tokens)]
         [expr1-ast (car expr1-res)]
         [rem1 (cdr expr1-res)]
         [op-tok (peek rem1)])
    (cond
      [(member op-tok '(EQ-TOK NOT-EQ-TOK GT-TOK GT-EQ-TOK LT-TOK LT-EQ-TOK))
       (let* ([rem2 (consume rem1 op-tok "Relational Operator")]
              [expr2-res (parse-expression rem2)]
              [expr2-ast (car expr2-res)]
              [rem3 (cdr expr2-res)]
              [op-sym (cond
                        [(equal? op-tok 'EQ-TOK) 'eq]
                        [(equal? op-tok 'NOT-EQ-TOK) 'neq]
                        [(equal? op-tok 'GT-TOK) 'gt]
                        [(equal? op-tok 'GT-EQ-TOK) 'gte]
                        [(equal? op-tok 'LT-TOK) 'lt]
                        [(equal? op-tok 'LT-EQ-TOK) 'lte])])
         (cons (list op-sym expr1-ast expr2-ast) rem3))]
      [else (syntax-error rem1 "comparison operator (=, !=, >, >=, <, <=)")])))

(define (parse-expression tokens)
  (let* ([term-res (parse-term tokens)]
         [term-ast (car term-res)]
         [rem (cdr term-res)])
    ;; Tail-recursive loop to enforce left-associativity
    (let loop ([current-ast term-ast] [current-rem rem])
      (let ([next-tok (peek current-rem)])
        (cond
          [(equal? next-tok 'ADD-TOK)
           (let* ([rem1 (consume current-rem 'ADD-TOK "\'+\'")]
                  [term2-res (parse-term rem1)]
                  [term2-ast (car term2-res)]
                  [rem2 (cdr term2-res)])
             (loop (list '+ current-ast term2-ast) rem2))]
          [(equal? next-tok 'SUB-TOK)
           (let* ([rem1 (consume current-rem 'SUB-TOK "\'-\'")]
                  [term2-res (parse-term rem1)]
                  [term2-ast (car term2-res)]
                  [rem2 (cdr term2-res)])
             (loop (list '- current-ast term2-ast) rem2))]
          [else (cons current-ast current-rem)])))))

(define (parse-term tokens)
  (let* ([factor-res (parse-factor tokens)]
         [factor-ast (car factor-res)]
         [rem (cdr factor-res)])
    (let loop ([current-ast factor-ast] [current-rem rem])
      (let ([next-tok (peek current-rem)])
        (cond
          [(equal? next-tok 'MUL-TOK)
           (let* ([rem1 (consume current-rem 'MUL-TOK "\'*\'")]
                  [factor2-res (parse-factor rem1)]
                  [factor2-ast (car factor2-res)]
                  [rem2 (cdr factor2-res)])
             (loop (list '* current-ast factor2-ast) rem2))]
          [(equal? next-tok 'DIV-TOK)
           (let* ([rem1 (consume current-rem 'DIV-TOK "\'/\'")]
                  [factor2-res (parse-factor rem1)]
                  [factor2-ast (car factor2-res)]
                  [rem2 (cdr factor2-res)])
             (loop (list '/ current-ast factor2-ast) rem2))]
          [else (cons current-ast current-rem)])))))

(define (parse-factor tokens)
  (let ([next-tok (peek tokens)])
    (cond
      ;; Variable ID
      [(and (list? next-tok) (equal? (first next-tok) 'ID-TOK))
       (let* ([val-res (consume-val tokens 'ID-TOK "Identifier")])
         (cons (string->symbol (car val-res)) (cdr val-res)))]
      
      ;; Integers
      [(and (list? next-tok) (equal? (first next-tok) 'INT-TOK))
       (let* ([val-res (consume-val tokens 'INT-TOK "Integer")])
         (cons (car val-res) (cdr val-res)))]
         
      ;; Floating Points
      [(and (list? next-tok) (equal? (first next-tok) 'FP-TOK))
       (let* ([val-res (consume-val tokens 'FP-TOK "Float")])
         (cons (car val-res) (cdr val-res)))]
         
      ;; Parenthetical nested expressions
      [(equal? next-tok 'START-PAR-TOK)
       (let* ([rem1 (consume tokens 'START-PAR-TOK "\"(\"")]
              [expr-res (parse-expression rem1)]
              [expr-ast (car expr-res)]
              [rem2 (consume (cdr expr-res) 'END-PAR-TOK "\")\"")])
         (cons expr-ast rem2))]
         
      ;; Unary Logic (Handling isolated negations like x := -5)
      [(equal? next-tok 'SUB-TOK)
       (let* ([rem1 (consume tokens 'SUB-TOK "\'-\'")]
              [fac-res (parse-factor rem1)]
              [fac-ast (car fac-res)]
              [rem2 (cdr fac-res)])
         (if (number? fac-ast)
             (cons (- fac-ast) rem2)
             (cons (list '* -1 fac-ast) rem2)))]
             
      [(equal? next-tok 'ADD-TOK)
       (let* ([rem1 (consume tokens 'ADD-TOK "\'+\'")]
              [fac-res (parse-factor rem1)])
         fac-res)]
         
      [else (syntax-error tokens "Identifier, Number, or \"(\"")])))


;---------------------------------------------------------------------------------------------------
(define source-file1 (read-file input-file1))
(define source-file2 (read-file input-file2))
(define source-file3 (read-file input-file3))
(define source-file4 (read-file input-file4))
(define source-file5 (read-file input-file5))
(define source-file6 (read-file input-file6))
(define source-file7 (read-file input-file7))

(define toks1 (tokenize source-file1))
(define toks2 (tokenize source-file2))
(define toks3 (tokenize source-file3))
(define toks4 (tokenize source-file4))
(define toks5 (tokenize source-file5))
(define toks6 (tokenize source-file6))
(define toks7 (tokenize source-file7))

(parse-program toks1)
;(parse-program toks2)
;(parse-program toks3)
;(parse-program toks4)
;(parse-program toks5)
;(parse-program toks6)
;(parse-program toks7)



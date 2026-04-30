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

(define (annotate-lines tokens [current-line 1] [acc '()])
  (cond
    [(empty? tokens) (reverse acc)]
    [(eq? (first tokens) 'NL-TOK)
     (annotate-lines (rest tokens) (+ current-line 1) acc)]
    [else
     (annotate-lines (rest tokens) current-line (cons (cons (first tokens) current-line) acc))]))

;; --- Core Utilities ---
(define (token-type t)
  (let ([raw (car t)])
    (if (list? raw) (first raw) raw)))

(define (token-val t)
  (let ([raw (car t)])
    (if (list? raw) (second raw) raw)))

(define (token-line t)
  (cdr t))

;; Helper to consume expected tokens, returning either 'ok or 'error
(define (expect type toks)
  (if (empty? toks)
      (list 'error "Unexpected EOF")
      (if (eq? (token-type (first toks)) type)
          (list 'ok (token-val (first toks)) (rest toks))
          (list 'error (format "Syntax error on line ~a: expected ~a, got ~a" 
                               (token-line (first toks)) type (token-type (first toks)))))))

;; --- AST Recursive Descent Parsers ---

(define (parse-program tokens)
  (let ([annotated-tokens (annotate-lines tokens)])
    (match (parse-stmt-list annotated-tokens)
      [(list 'error msg) msg] ; Return the error string directly
      [(list 'ok ast rem)
       (if (or (empty? rem) (eq? (token-type (first rem)) 'EOF-TOK))
           ast ; Success! Return the full AST list.
           (format "Syntax error on line ~a: expected EOF, got ~a" 
                   (token-line (first rem)) (token-type (first rem))))])))

(define (parse-stmt-list tokens)
  (let loop ([toks tokens] [asts '()])
    (if (empty? toks)
        (list 'ok (reverse asts) toks)
        (let ([ttype (token-type (first toks))])
          (if (member ttype '(ID-TOK IF-TOK WHILE-TOK PRINT-TOK))
              (match (parse-statement toks)
                [(list 'error msg) (list 'error msg)]
                [(list 'ok stmt-ast rem-toks)
                 (loop rem-toks (cons stmt-ast asts))])
              ;; Epsilon transition for end of block/program
              (list 'ok (reverse asts) toks))))))

(define (parse-statement tokens)
  (if (empty? tokens)
      (list 'error "Unexpected EOF in statement")
      (case (token-type (first tokens))
        [(ID-TOK)    (parse-assign-stmt tokens)]
        [(IF-TOK)    (parse-if-stmt tokens)]
        [(WHILE-TOK) (parse-while-stmt tokens)]
        [(PRINT-TOK) (parse-print-stmt tokens)]
        [else (list 'error (format "Unexpected token ~a starting statement on line ~a" 
                                   (token-type (first tokens)) (token-line (first tokens))))])))

(define (parse-assign-stmt tokens)
  (match (expect 'ID-TOK tokens)
    [(list 'error msg) (list 'error msg)]
    [(list 'ok id-val r1)
     (match (expect 'ASSIGN-TOK r1)
       [(list 'error msg) (list 'error msg)]
       [(list 'ok _ r2)
        (match (parse-expression r2)
          [(list 'error msg) (list 'error msg)]
          [(list 'ok expr-ast r3)
           (match (expect 'SEMI-TOK r3)
             [(list 'error msg) (list 'error msg)]
             [(list 'ok _ r4)
              ;; Converts "x" to 'x to match your expected output
              (list 'ok (list 'assign (string->symbol id-val) expr-ast) r4)])])])]))

(define (parse-if-stmt tokens)
  (match (expect 'IF-TOK tokens)
    [(list 'error msg) (list 'error msg)]
    [(list 'ok _ r1)
     (match (parse-comparison r1)
       [(list 'error msg) (list 'error msg)]
       [(list 'ok cond-ast r2)
        (match (expect 'THEN-TOK r2)
          [(list 'error msg) (list 'error msg)]
          [(list 'ok _ r3)
           (match (parse-stmt-list r3)
             [(list 'error msg) (list 'error msg)]
             [(list 'ok then-asts r4)
              (match (expect 'ELSE-TOK r4)
                [(list 'error msg) (list 'error msg)]
                [(list 'ok _ r5)
                 (match (parse-stmt-list r5)
                   [(list 'error msg) (list 'error msg)]
                   [(list 'ok else-asts r6)
                    (match (expect 'END-BLK-TOK r6)
                      [(list 'error msg) (list 'error msg)]
                      [(list 'ok _ r7)
                       (match (expect 'SEMI-TOK r7)
                         [(list 'error msg) (list 'error msg)]
                         [(list 'ok _ r8)
                          (list 'ok (list 'if cond-ast then-asts else-asts) r8)])])])])])])])]))

(define (parse-while-stmt tokens)
  (match (expect 'WHILE-TOK tokens)
    [(list 'error msg) (list 'error msg)]
    [(list 'ok _ r1)
     (match (parse-comparison r1)
       [(list 'error msg) (list 'error msg)]
       [(list 'ok cond-ast r2)
        (match (expect 'DO-TOK r2)
          [(list 'error msg) (list 'error msg)]
          [(list 'ok _ r3)
           (match (parse-stmt-list r3)
             [(list 'error msg) (list 'error msg)]
             [(list 'ok body-asts r4)
              (match (expect 'END-BLK-TOK r4)
                [(list 'error msg) (list 'error msg)]
                [(list 'ok _ r5)
                 (match (expect 'SEMI-TOK r5)
                   [(list 'error msg) (list 'error msg)]
                   [(list 'ok _ r6)
                    (list 'ok (list 'while cond-ast body-asts) r6)])])])])])]))

(define (parse-print-stmt tokens)
  (match (expect 'PRINT-TOK tokens)
    [(list 'error msg) (list 'error msg)]
    [(list 'ok _ r1)
     (match (parse-expression r1)
       [(list 'error msg) (list 'error msg)]
       [(list 'ok expr-ast r2)
        (match (expect 'SEMI-TOK r2)
          [(list 'error msg) (list 'error msg)]
          [(list 'ok _ r3)
           (list 'ok (list 'print expr-ast) r3)])])]))

(define (parse-comparison tokens)
  (match (parse-expression tokens)
    [(list 'error msg) (list 'error msg)]
    [(list 'ok left-ast r)
     (if (empty? r)
         (list 'error "Expected comparison operator")
         (let ([ttype (token-type (first r))])
           (if (member ttype '(EQ-TOK NOT-EQ-TOK GT-TOK GT-EQ-TOK LT-TOK LT-EQ-TOK))
               (match (parse-expression (rest r))
                 [(list 'error msg) (list 'error msg)]
                 [(list 'ok right-ast r2)
                  (let ([op (case ttype
                              [(EQ-TOK) '=] [(NOT-EQ-TOK) '!=]
                              [(GT-TOK) '>] [(GT-EQ-TOK) '>=]
                              [(LT-TOK) '<] [(LT-EQ-TOK) '<=])])
                    (list 'ok (list op left-ast right-ast) r2))])
               (list 'error (format "Expected comparison operator on line ~a" (token-line (first r)))))))]))

(define (parse-expression tokens)
  (match (parse-term tokens)
    [(list 'error msg) (list 'error msg)]
    [(list 'ok left-ast r)
     ;; Loop handles left-associativity for (+ | -)
     (let loop ([ast left-ast] [toks r])
       (if (empty? toks)
           (list 'ok ast toks)
           (let ([ttype (token-type (first toks))])
             (if (member ttype '(ADD-TOK SUB-TOK))
                 (match (parse-term (rest toks))
                   [(list 'error msg) (list 'error msg)]
                   [(list 'ok right-ast r2)
                    (let ([op (if (eq? ttype 'ADD-TOK) '+ '-)])
                      (loop (list op ast right-ast) r2))])
                 (list 'ok ast toks)))))]))

(define (parse-term tokens)
  (match (parse-factor tokens)
    [(list 'error msg) (list 'error msg)]
    [(list 'ok left-ast r)
     ;; Loop handles left-associativity for (* | /)
     (let loop ([ast left-ast] [toks r])
       (if (empty? toks)
           (list 'ok ast toks)
           (let ([ttype (token-type (first toks))])
             (if (member ttype '(MUL-TOK DIV-TOK))
                 (match (parse-factor (rest toks))
                   [(list 'error msg) (list 'error msg)]
                   [(list 'ok right-ast r2)
                    (let ([op (if (eq? ttype 'MUL-TOK) '* '/)])
                      (loop (list op ast right-ast) r2))])
                 (list 'ok ast toks)))))]))

(define (parse-factor tokens)
  (if (empty? tokens)
      (list 'error "Unexpected EOF in factor")
      (let ([t (first tokens)] [rem (rest tokens)])
        (case (token-type t)
          [(INT-TOK FP-TOK) (list 'ok (token-val t) rem)]
          [(ID-TOK)         (list 'ok (string->symbol (token-val t)) rem)]
          [(ADD-TOK)        (parse-factor rem)] ; Handle unary positive
          [(SUB-TOK)        ; Handle unary minus
           (match (parse-factor rem)
             [(list 'error msg) (list 'error msg)]
             [(list 'ok val r2) (list 'ok (list '- 0 val) r2)])]
          [(START-PAR-TOK)
           (match (parse-expression rem)
             [(list 'error msg) (list 'error msg)]
             [(list 'ok expr-ast r2)
              (match (expect 'END-PAR-TOK r2)
                [(list 'error msg) (list 'error msg)]
                [(list 'ok _ r3) (list 'ok expr-ast r3)])])]
          [else (list 'error (format "Syntax error on line ~a: unexpected token ~a" 
                                     (token-line t) (token-type t)))]))))

;---------------------------------------------------------------------------------------------------
(define source-file (read-file input-file))
(displayln (split-string source-file))
(define toks (tokenize source-file))
(print toks) (newline)
(parse-program toks)



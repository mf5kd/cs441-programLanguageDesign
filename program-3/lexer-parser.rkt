#lang racket

;; ======================================================================
;; THE SCANNER (LEXER) - Character-by-Character State Machine
;; ======================================================================

(define (read-file input-file)
  (file->string input-file))

;; Helper to determine if a '+' or '-' should be binary or unary
(define (is-binary? tokens)
  (let loop ([toks tokens])
    (cond
      [(empty? toks) #f]
      [(equal? (first toks) 'NL-TOK) (loop (rest toks))] ; Skip newlines when checking
      [else
       (let ([prev (first toks)])
         (cond
           [(equal? prev 'END-PAR-TOK) #t]
           [(and (list? prev) (member (first prev) '(ID-TOK INT-TOK FP-TOK))) #t]
           [else #f]))])))

;; Helper to skip block comments
(define (skip-block-comment chars)
  (cond
    [(empty? chars) '()] ; Reached end of file inside comment
    [(and (char=? (first chars) #\*) (not (empty? (rest chars))) (char=? (second chars) #\/))
     (rest (rest chars))] ; Consume the '*/' and return the rest
    [else (skip-block-comment (rest chars))]))

;; Helper to read Identifiers and Keywords
(define (read-id chars acc)
  (cond
    [(empty? chars) (values (list->string (reverse acc)) chars)]
    [(or (char-alphabetic? (first chars))
         (char-numeric? (first chars))
         (char=? (first chars) #\_)
         (char=? (first chars) #\-))
     (read-id (rest chars) (cons (first chars) acc))]
    [else (values (list->string (reverse acc)) chars)]))

(define (classify-word word)
  (cond
    [(equal? word "IF") 'IF-TOK]
    [(equal? word "THEN") 'THEN-TOK]
    [(equal? word "ELSE") 'ELSE-TOK]
    [(equal? word "WHILE") 'WHILE-TOK]
    [(equal? word "DO") 'DO-TOK]
    [(equal? word "END") 'END-BLK-TOK]
    [(equal? word "PRINT") 'PRINT-TOK]
    [else (list 'ID-TOK word)]))

;; Helper to read Numbers (INT and FP)
(define (read-number chars acc has-decimal? pending-sign)
  (cond
    [(empty? chars) (values (format-number acc has-decimal? pending-sign) chars)]
    
    ;; CATCHES THE "3Y" ERROR: A letter right after/inside a number!
    [(char-alphabetic? (first chars))
     (error "Lexical Error: Invalid identifier starting with a digit or malformed number.")]
     
    [(char-numeric? (first chars))
     (read-number (rest chars) (cons (first chars) acc) has-decimal? pending-sign)]
     
    [(char=? (first chars) #\.)
     (if has-decimal?
         (error "Lexical Error: FP cannot have multiple decimal points.")
         (read-number (rest chars) (cons (first chars) acc) #t pending-sign))]
         
    [else (values (format-number acc has-decimal? pending-sign) chars)]))

(define (format-number acc has-decimal? pending-sign)
  (let* ([raw-str (list->string (reverse acc))] ; String without the sign (for zero checks)
         [num-str (string-append pending-sign raw-str)] ; Full string
         [num-val (string->number num-str)])
    (if has-decimal?
        (if (or (string-prefix? raw-str ".") (string-suffix? raw-str "."))
            (error "Lexical Error: FP must have at least one digit before and after decimal.")
            (list 'FP-TOK num-val))
        (if (and (> (string-length raw-str) 1) (char=? (string-ref raw-str 0) #\0))
            (error (format "Lexical Error: INT cannot start with a leading zero (~a)" raw-str))
            (list 'INT-TOK num-val)))))

;; Main Lexer Entry Point
(define (tokenize source-string)
  (tokenize-chars (string->list source-string) '() ""))

(define (tokenize-chars chars tokens pending-sign)
  (cond
    ;; Base Case: EOF
    [(empty? chars)
     (if (non-empty-string? pending-sign)
         (error "Lexical Error: Unexpected end of file after unary sign")
         (reverse (cons 'EOF-TOK tokens)))]

    [else
     (let ([c (first chars)]
           [rest-chars (rest chars)])
       (cond
         ;; --- Whitespace ---
         [(char-whitespace? c)
          (if (char=? c #\newline)
              (tokenize-chars rest-chars (cons 'NL-TOK tokens) pending-sign)
              (tokenize-chars rest-chars tokens pending-sign))]

         ;; --- Comments /* ... */ ---
         [(and (char=? c #\/) (not (empty? rest-chars)) (char=? (first rest-chars) #\*))
          (let ([rem-chars (skip-block-comment (rest rest-chars))])
            (tokenize-chars rem-chars tokens pending-sign))]

         ;; --- Identifiers and Keywords ---
         [(or (char-alphabetic? c) (char=? c #\_))
          (if (non-empty-string? pending-sign)
              (error (format "Lexical Error: Unary sign '~a' must be followed by a number." pending-sign))
              (let-values ([(word rem-chars) (read-id chars '())])
                (tokenize-chars rem-chars (cons (classify-word word) tokens) "")))]

         ;; --- Numbers (INT and FP) ---
         [(char-numeric? c)
          (let-values ([(num-tok rem-chars) (read-number chars '() #f pending-sign)])
            (tokenize-chars rem-chars (cons num-tok tokens) ""))]

         ;; --- Multi-Character Operators (:=, !=, >=, <=) ---
         [(char=? c #\:)
          (if (and (not (empty? rest-chars)) (char=? (first rest-chars) #\=))
              (tokenize-chars (rest rest-chars) (cons 'ASSIGN-TOK tokens) pending-sign)
              (error "Lexical Error: Expected '=' after ':'"))]
              
         [(char=? c #\!)
          (if (and (not (empty? rest-chars)) (char=? (first rest-chars) #\=))
              (tokenize-chars (rest rest-chars) (cons 'NOT-EQ-TOK tokens) pending-sign)
              (error "Lexical Error: Unrecognized symbol '!'"))]
              
         [(char=? c #\>)
          (if (and (not (empty? rest-chars)) (char=? (first rest-chars) #\=))
              (tokenize-chars (rest rest-chars) (cons 'GT-EQ-TOK tokens) pending-sign)
              (tokenize-chars rest-chars (cons 'GT-TOK tokens) pending-sign))]
              
         [(char=? c #\<)
          (if (and (not (empty? rest-chars)) (char=? (first rest-chars) #\=))
              (tokenize-chars (rest rest-chars) (cons 'LT-EQ-TOK tokens) pending-sign)
              (tokenize-chars rest-chars (cons 'LT-TOK tokens) pending-sign))]

         ;; --- Single-Character Operators & Punctuation ---
         [(char=? c #\() (tokenize-chars rest-chars (cons 'START-PAR-TOK tokens) pending-sign)]
         [(char=? c #\)) (tokenize-chars rest-chars (cons 'END-PAR-TOK tokens) pending-sign)]
         [(char=? c #\;) (tokenize-chars rest-chars (cons 'SEMI-TOK tokens) pending-sign)]
         [(char=? c #\*) (tokenize-chars rest-chars (cons 'MUL-TOK tokens) pending-sign)]
         [(char=? c #\/) (tokenize-chars rest-chars (cons 'DIV-TOK tokens) pending-sign)]
         [(char=? c #\=) (tokenize-chars rest-chars (cons 'EQ-TOK tokens) pending-sign)]

         ;; --- +/- (Unary vs Binary logic) ---
         [(char=? c #\+)
          (if (is-binary? tokens)
              (tokenize-chars rest-chars (cons 'ADD-TOK tokens) pending-sign)
              (tokenize-chars rest-chars tokens "+"))]
              
         [(char=? c #\-)
          (if (is-binary? tokens)
              (tokenize-chars rest-chars (cons 'SUB-TOK tokens) pending-sign)
              (tokenize-chars rest-chars tokens "-"))]

         ;; --- Catch-All ---
         [else (error (format "Lexical Error: Unrecognized character ~a" c))]))]))


;; ======================================================================
;; STATE MANAGEMENT & PARSER HELPERS
;; ======================================================================

(define current-orig-tokens (make-parameter '()))

(define (get-line original-tokens current-tokens)
  (let loop ([toks original-tokens] [line 1])
    (cond
      [(eq? toks current-tokens) line] 
      [(empty? toks) line]
      [(equal? (first toks) 'NL-TOK) (loop (rest toks) (+ line 1))]
      [else (loop (rest toks) line)])))

(define (skip-newlines tokens)
  (cond
    [(empty? tokens) '()]
    [(equal? (first tokens) 'NL-TOK) (skip-newlines (rest tokens))]
    [else tokens]))

(define (peek tokens)
  (let ([cleaned (skip-newlines tokens)])
    (if (empty? cleaned)
        'EOF-TOK
        (first cleaned))))

(define (syntax-error tokens expected-str)
  (let ([line (get-line (current-orig-tokens) tokens)])
    (raise-user-error (format "SYNTAX ERROR missing or expected ~a at line ~a" expected-str line))))

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

(define (consume-val tokens expected-type err-msg)
  (let ([cleaned (skip-newlines tokens)])
    (if (empty? cleaned)
        (syntax-error tokens err-msg)
        (let ([tok (first cleaned)])
          (if (and (list? tok) (equal? (first tok) expected-type))
              (cons (second tok) (rest cleaned))
              (syntax-error tokens err-msg))))))


;; ======================================================================
;; THE RECURSIVE DESCENT PARSER (Unchanged)
;; ======================================================================

(define (parse-program tokens)
  (parameterize ([current-orig-tokens tokens])
    (let* ([res (parse-stmt-list tokens)]
           [ast (car res)]
           [rem (cdr res)]
           [final-rem (skip-newlines rem)])
      (if (or (empty? final-rem) (equal? (first final-rem) 'EOF-TOK))
          (cons 'program ast)
          (syntax-error final-rem "EOF")))))

(define (parse-stmt-list tokens)
  (let* ([res1 (parse-statement tokens)]
         [ast1 (car res1)]
         [rem1 (cdr res1)]
         [next-tok (peek rem1)])
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
      [(and (list? next-tok) (equal? (first next-tok) 'ID-TOK))
       (let* ([val-res (consume-val tokens 'ID-TOK "Identifier")])
         (cons (string->symbol (car val-res)) (cdr val-res)))]
      
      [(and (list? next-tok) (equal? (first next-tok) 'INT-TOK))
       (let* ([val-res (consume-val tokens 'INT-TOK "Integer")])
         (cons (car val-res) (cdr val-res)))]
         
      [(and (list? next-tok) (equal? (first next-tok) 'FP-TOK))
       (let* ([val-res (consume-val tokens 'FP-TOK "Float")])
         (cons (car val-res) (cdr val-res)))]
         
      [(equal? next-tok 'START-PAR-TOK)
       (let* ([rem1 (consume tokens 'START-PAR-TOK "\"(\"")]
              [expr-res (parse-expression rem1)]
              [expr-ast (car expr-res)]
              [rem2 (consume (cdr expr-res) 'END-PAR-TOK "\")\"")])
         (cons expr-ast rem2))]
         
      [else (syntax-error tokens "Identifier, Number, or \"(\"")])))

;; ======================================================================
;; TESTING 
;; ======================================================================
;; List of the files you currently want to test
(define test-files '("input/Input1.txt" 
                     "input/Input2.txt" 
                     "input/Input3.txt" ; Let's pretend this one has an error
                     "input/Input4.txt"
                     "input/Input5.txt"
                     "input/Input6.txt"
                     "input/Input7.txt"))

;; A helper function to run the full pipeline on a single file
(define (run-test file-path)
  (displayln (format "=== Testing ~a ===" file-path))
  
  ;; The with-handlers block catches any crashes inside it
  (with-handlers ([exn:fail? (lambda (exn)
                               ;; This runs ONLY if an error is thrown
                               (displayln (format "  --> FAILED: ~a\n" (exn-message exn))))])
    ;; The "Try" block: Run the normal pipeline
    (let* ([source (read-file file-path)]
           [tokens (tokenize source)]
           [ast    (parse-program tokens)])
      (displayln "AST Output:")
      (pretty-print ast)
      (displayln ""))))

;; Run the pipeline for every file in the list
(for-each run-test test-files)

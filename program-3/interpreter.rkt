#lang racket
(require "lexer-parser.rkt") ;; NOTE imports read-file tokenize parse-program

(define (get-value value environment)
  (cond 
    [(list? value) (eval-expr value environment)]  ; if list it must be math that needs to be evaluated 
    [(symbol? value) 
     (hash-ref environment value 
               (lambda () (error "Unbound Variable Error: ~a has not been assigned." value)))] ; if symbol it is a var reference and must be looked up in the environment
    [else value]))

(define (exec-stmt abstract-syntax-tree environment)
  (if (empty? abstract-syntax-tree) 
      environment
      (let* ([statement (first abstract-syntax-tree)]
             [new-tree (rest abstract-syntax-tree)]
            [command (first statement)]
            [v1 (second statement)]
            [v2 (if (< (length statement) 3) 
                    empty
                    (third statement))])
        (cond 
          [(equal? command 'assign) (exec-stmt new-tree (hash-set environment v1 (get-value v2 environment)))]
          [(equal? command 'print) (displayln (get-value v1 environment)) (exec-stmt new-tree environment)]
          [(equal? command 'if) 
           (let* ([condition (eval-comp (second statement) environment)]
                  [branch-ast (if condition 
                                  (third statement)    ; THEN block
                                  (fourth statement))] ; ELSE block
                  [branch-env (exec-stmt branch-ast environment)])
             (exec-stmt new-tree branch-env))]
          [(equal? command 'while)
           (let ([post-loop-env (eval-while (second statement) (rest (rest statement)) environment)]) 
             (exec-stmt new-tree post-loop-env))]
          [else new-tree]))))

(define (eval-while condition-ast body-ast environment)
  (cond 
    [(eval-comp condition-ast environment)
     (define new-env (exec-stmt body-ast environment))
     (eval-while condition-ast body-ast new-env)]
    [else environment]))

(define (eval-expr expr env)
  (cond
    [(number? expr) expr]
    [(symbol? expr)
     (hash-ref env expr
               (lambda () (error "Unbound Variable Error: ~a has not been assigned." expr)))]
    [(list? expr)
     (let ([op (first expr)]
           [v1 (eval-expr (second expr) env)]
           [v2 (if (> (length expr) 2) 
                   (eval-expr (third expr) env)
                   #f)])
       (cond
         [(equal? op '+) (+ v1 v2)]
         [(equal? op '-) (if v2 (- v1 v2) (- v1))] ;; Handles both binary and unary minus
         [(equal? op '*) (* v1 v2)]
         [(equal? op '/) 
          (if (= v2 0)
              (error "Division by zero") ;; Good practice to catch this
              (/ v1 v2))]
         [else (error "Unknown operator: ~a" op)]))]
    [else (error "Invalid expression structure: ~a" expr)]))

(define (symbol->operator s)
  (define (!= x y) (not (= x y)))
  (match s 
    ['gt >]['lt <]['gte >=]['lte <=]['eq =]['neq !=]))

(define (eval-comp statement environment)
  (let ([op (symbol->operator (first statement))]
        [v1 (get-value (second statement) environment)]
        [v2 (get-value (third statement) environment)])
    (if (and (number? v1) (number? v2))
        (op v1 v2)
        (error "Comparison requires numeric operands"))))

(define (interpret abstract-syntax-tree environment)
  (define first-node (first abstract-syntax-tree))
  (cond 
    [(empty? abstract-syntax-tree) (display "")]
    [(equal? first-node 'program) (interpret (rest abstract-syntax-tree) environment)]
    [else (exec-stmt abstract-syntax-tree environment)]))



; TESTING

(define test-files '("input/scope-test.txt"
                     "input/Input1.txt"
                     "input/Input2.txt"
                     "input/Input3.txt"
                     "input/Input4.txt"
                     "input/Input5.txt"
                     "input/Input6.txt"
                     "input/Input7.txt"))

(define (run-test file-path)
  (displayln (format "\n=== Running ~a ===" file-path))
  
  (with-handlers ([exn:fail? (lambda (exn)
                               (displayln (format "  --> CAUGHT ERROR: ~a\n" (exn-message exn))))])
    (let* ([source-code (read-file file-path)]
           [tokens      (tokenize source-code)]
           [ast         (parse-program tokens)]
           [empty-env   (make-immutable-hash '())]) ; Always start fresh for each file

      (displayln "Generated AST:")
      (pretty-print ast)
      (displayln "Execution Output:")

      (displayln (interpret ast empty-env))
      (displayln "\n  --> Execution Successful"))))

(for-each run-test test-files)

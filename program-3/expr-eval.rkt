#lang racket
;; Constant Folding
(define (fold-constants expr)
  (match expr
    [`(+ ,(? number? a) ,(? number? b)) (+ a b)]
    [`(- ,(? number? a) ,(? number? b)) (- a b)]
    [`(* ,(? number? a) ,(? number? b)) (* a b)]
    [`(/ ,(? number? a) ,(? number? b))
     (if (= b 0) expr (/ a b))]
    [_ expr]))

(define (canonicalize expr)
  (match expr
    [`(+ ,(? number? n) ,v) #:when (not (number? v)) `(+ ,v ,n)]
    [`(* ,(? number? n) ,v) #:when (not (number? v)) `(* ,v ,n)]
    [_ expr]))

(define (group-terms expr)
  (match expr
    ; Addition grouping
    [`(+ (+ ,var ,(? number? a)) ,(? number? b))
     `(+ ,var ,(+ a b))]
    [`(+ ,(? number? a) (+ ,var ,(? number? b)))
     `(+ ,var ,(+ a b))]
    [`(+ (+ ,var1 ,(? number? a)) (+ ,var2 ,(? number? b)))
     `(+ (+ ,var1 ,(+ a b)) ,var2)]

    ; Multiplication grouping
    [`(* (* ,var ,(? number? a)) ,(? number? b))
     `(* ,var ,(* a b))]
    [`(* ,(? number? a) (* ,var ,(? number? b)))
     `(* ,var ,(* a b))]
    [`(* (* ,var1 ,(? number? a)) (* ,var2 ,(? number? b)))
     `(* (* ,var1 ,(* a b)) ,var2)]

    [_ expr]))

; Algebraic Identities (Pattern Matcher)
(define (reduce-identities expr)
  (match expr
    ; Addition Identities
    [`(+ ,x 0) x]
    [`(+ 0 ,x) x]

    ; Multiplication Identities
    [`(* ,x 1) x]
    [`(* 1 ,x) x]

    ; Zero Properties
    [`(* ,x 0) 0]
    [`(* 0 ,x) 0]

    ; Double Negation
    [`(- (- ,x)) x]

    ; Like-Term Extraction
    [`(+ (* ,var ,(? number? a)) (* ,var ,(? number? b)))
     `(* ,var ,(+ a b))]

    [_ expr]))

; Single Pass Simplification
(define (simplify-step expr env)
  (cond
    [(number? expr) expr]
    ; If it's a symbol, substitute from environment or throw an error
    [(symbol? expr)
     (hash-ref env expr
               (lambda () (error 'simplify "Unbound variable: ~a" expr)))]
    [(not (list? expr)) expr]
    [else
     ; Recursively simplify (we skip the operator using `rest`)
     (let* ([op (first expr)]
            [args (map (lambda (e) (simplify-step e env)) (rest expr))]
            [new-expr (cons op args)])
       ; Apply rules
       (reduce-identities
        (group-terms
         (fold-constants
          (canonicalize new-expr)))))]))

; Fixed-Point Iteration
(define (simplify expr env)
  (let ([next (simplify-step expr env)])
    (if (equal? expr next)
        expr
        (simplify next env))))

(provide simplify)

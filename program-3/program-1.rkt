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
(define (simplify-step expr)
  (cond
    [(not (list? expr)) expr]
    [else
     ; Recursively simplify
     (let* ([op (first expr)]
            [args (map simplify-step (rest expr))]
            [new-expr (cons op args)])
       ; Apply rules
       (reduce-identities 
        (group-terms 
         (fold-constants 
          (canonicalize new-expr)))))]))

; Fixed-Point Iteration
(define (simplify expr)
  (let ([next (simplify-step expr)])
    (if (equal? expr next)
        expr
        (simplify next))))

; Test Cases 
(define l1 '(+ (+ 2 2) (* 5 6)))                     ; -> 34
(define l2 '(+ (+ 2 x) (+ 3 1)))                     ; -> (+ x 6)
(define l3 '(+ (+ 2 x) (+ 3 y)))                     ; -> (+ (+ x 5) y)
(define l4 '(* (* 2 x) (* 3 y)))                     ; -> (* (* x 6) y)
(define l5 '(* (- 5 5) (+ x y)))                     ; -> 0
(define l6 '(+ (* 1 x) 0))                           ; -> x
(define l7 '(- 20 (/ (+ (* 4 5) (* 2 5)) (- 8 2))))  ; -> 15
(define l8 '(+ x 0))                                 ; -> x
(define l9 '(+ 5 (+ 10 2)))                          ; -> 17
(define l10 '(+ (* 1 x) (* y 0)))                    ; -> x
(define l11 '(+ 0 (+ 0 (+ 0 z))))                    ; -> z
(define l12 '(* (+ 1 0) (+ x 0)))                    ; -> x
(define l13 '(+ (* 1 x) (+ 0 y)))                    ; -> (+ x y)
(define l14 '(- 20 (/ (+ (* 4 5) (* 2 5)) (- 8 2)))) ; -> 15
(define l15 '(+ (* 2 x) (* 3 x)))                    ; -> (* x 5)

(define (run-tests)
  (for-each (lambda (test)
              (printf "~a = ~a\n" test (simplify test)))
            (list l1 l2 l3 l4 l5 l6 l7 l8 l9 l10 l11 l12 l13 l14 l15)))

(run-tests)

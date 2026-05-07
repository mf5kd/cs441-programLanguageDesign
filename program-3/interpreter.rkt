#lang racket
(require "program-2.rkt")
(require "program-1.rkt")
(define file-path "input/Input1.txt")
(define source-code (read-file file-path))
(define tokens (tokenize source-code))
(define ast (parse-program tokens))

(simplify '(* x (+ 2 x)))

(define env (make-immutable-hash '()))



#lang racket
(require "program-2.rkt")
(define file-path "input/Input1.txt")
(define source-code (read-file file-path))
(define tokens (tokenize source-code))
(define ast (parse-program tokens))

ast

(define env (make-immutable-hash '()))



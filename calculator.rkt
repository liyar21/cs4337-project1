#lang racket
(require "mode.rkt")

(define (to-number s)
  (string->number s))

;; Simple evaluator for +, -, *, /
(define (eval-line line)
  (define tokens (string-split line))
  (cond
    [(= (length tokens) 3)
     (define op (list-ref tokens 0))
     (define a (to-number (list-ref tokens 1)))
     (define b (to-number (list-ref tokens 2)))
     (cond
       [(string=? op "+") (+ a b)]
       [(string=? op "-") (- a b)]
       [(string=? op "*") (* a b)]
       [(string=? op "/")
        (if (zero? b)
            "Error: divide by zero"
            (/ a b))]
       [else "Unsupported operator"])]
    [else "Invalid format, must be: + a b or - a b or * a b or / a b etc."]))

(define (repl hist)
  (when prompt? (display "> "))
  (define line (read-line))
  (cond
    [(eof-object? line) (void)]
    [(string=? line "quit") (void)]
    [else
     (define result (eval-line line))
     (displayln result)
     (repl hist)]))

(repl '())


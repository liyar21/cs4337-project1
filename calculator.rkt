#lang racket
(require "mode.rkt")

;; Convert string input into a number if possible
(define (to-number s)
  (string->number s))

;; Remove empty tokens caused by extra spaces
(define (clean-tokens tokens)
  (filter (lambda (x) (not (string=? x ""))) tokens))

;; Evaluate expressions (+ a b), (- a b), etc.
(define (eval-line line)
  (define raw-tokens (string-split line " "))
  (define tokens (clean-tokens raw-tokens))
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
    [else "Invalid input or missing arguments"]))

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


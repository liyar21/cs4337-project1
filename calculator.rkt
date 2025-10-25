#lang racket
(require "mode.rkt")

(define (to-number s)
  (string->number s))

(define (clean-tokens tokens)
  (filter (lambda (x) (not (string=? x ""))) tokens))

;;$1 gives latest
(define (get-history hist n)
  (if (and (integer? n)
           (>= n 1)
           (<= n (length hist)))
      (list-ref hist (- n 1)) ; should have used (reverse hist)
      (error 'history "Invalid $ index")))

;; Evaluate expressions, including $n references
(define (eval-line line hist)
  (define tokens (clean-tokens (string-split line " ")))
  (cond
    [(= (length tokens) 3)
     (define op (list-ref tokens 0))
     (define a-str (list-ref tokens 1))
     (define b-str (list-ref tokens 2))
     (define a (if (string-prefix? "$" a-str)
                   (get-history hist (string->number (substring a-str 1)))
                   (to-number a-str)))
     (define b (if (string-prefix? "$" b-str)
                   (get-history hist (string->number (substring b-str 1)))
                   (to-number b-str)))
     (cond
       [(string=? op "+") (+ a b)]
       [(string=? op "-") (- a b)]
       [(string=? op "*") (* a b)]
       [(string=? op "/")
        (if (zero? b)
            "Error: divide by zero"
            (/ a b))]
       [else "Unsupported operator"])]
    [else "Invalid input format"]))

;;update history list each loop
(define (repl hist)
  (when prompt? (display "> "))
  (define line (read-line))
  (cond
    [(eof-object? line) (void)]
    [(string=? line "quit") (void)]
    [else
     (define result (with-handlers ([exn:fail? (lambda (e) "Error: bad expression")])
                      (eval-line line hist)))
     (displayln result)
     (repl hist)])) 

(repl '())


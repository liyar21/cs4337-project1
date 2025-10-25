#lang racket
;; Version 6 - Functional history tracking and proper float output
;; Goal: follow project requirements for history, ids, and float display
;; History is passed as parameter and updated with cons each time.

(require "mode.rkt")

;; ---------- Helper Functions ----------
(define (to-float n) (real->double-flonum n))

(define (to-number s)
  (string->number s))

(define (clean-tokens tokens)
  (filter (lambda (x) (not (string=? x ""))) tokens))

;; Retrieve value from history list using 1-based id
(define (get-history hist id)
  (define rev (reverse hist))
  (if (and (integer? id)
           (>= id 1)
           (<= id (length rev)))
      (list-ref rev (- id 1))
      (error 'history "invalid id")))

;; ---------- Evaluator ----------
(define (eval-line line hist)
  (define tokens (clean-tokens (string-split line " ")))
  (cond
    [(= (length tokens) 3)
     (define op (list-ref tokens 0))
     (define a-str (list-ref tokens 1))
     (define b-str (list-ref tokens 2))

     ;; Support $n for previous results
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
            (error 'eval "divide by zero")
            (/ a b))]
       [else (error 'parse "unsupported operator")])]
    [else (error 'parse "invalid format")]))

;; ---------- Main Loop ----------
(define (repl hist)
  (when prompt? (display "> "))
  (define line (read-line))
  (cond
    [(eof-object? line) (void)]
    [(string=? line "quit") (void)]
    [else
     (with-handlers ([exn:fail?
                      (lambda (e)
                        (displayln "Error: Invalid Expression")
                        (repl hist))])
       (define result (eval-line line hist))
       (define new-hist (cons result hist))
       (define id (length (reverse new-hist)))
       (display id)
       (display ": ")
       (display (to-float result))
       (newline)
       (repl new-hist))]))

(repl '())


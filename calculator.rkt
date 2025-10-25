#lang racket
(require "mode.rkt")

;; ---------- Helper Functions ----------
(define (to-float n) (real->double-flonum n))

(define (to-number s)
  (string->number (string-trim s)))

(define (clean-tokens tokens)
  (filter (lambda (x) (not (string=? x ""))) (map string-trim tokens)))

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
  (define tokens (clean-tokens (string-split line)))
  (cond
    [(= (length tokens) 3)
     (define op (list-ref tokens 0))
     (define a-str (string-trim (list-ref tokens 1)))
     (define b-str (string-trim (list-ref tokens 2)))

     ;; --- Handle $ history references safely ---
     (define a
       (if (and (> (string-length a-str) 1)
                (char=? (string-ref a-str 0) #\$))
           (get-history hist (string->number (substring a-str 1)))
           (to-number a-str)))

     (define b
       (if (and (> (string-length b-str) 1)
                (char=? (string-ref b-str 0) #\$))
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


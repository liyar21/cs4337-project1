#lang racket
;; Version 7 — Recursive Prefix Evaluator (Progressive upgrade)
;; -------------------------------------------------------------
;; Builds on Version 6.2
;; Adds full prefix parsing (nested +, *, /, unary -)
;; Adds $n history recall and integer division (quotient)
;; Follows all CS4337 Project 1 requirements

(require "mode.rkt")

;; ---------- Helper Functions ----------
(define (to-float n) (real->double-flonum n))

(define (skip-space chars)
  (cond [(null? chars) chars]
        [(char-whitespace? (car chars)) (skip-space (cdr chars))]
        [else chars]))

(define (read-digits chars)
  (let loop ([c chars] [acc '()])
    (cond
      [(and (pair? c) (char-numeric? (car c)))
       (loop (cdr c) (cons (car c) acc))]
      [else
       (if (null? acc)
           #f
           (cons (string->number (list->string (reverse acc))) c))])))

(define (read-int chars)
  (define chars* (skip-space chars))
  (cond
    [(null? chars*) #f]
    [else
     (define first (car chars*))
     (cond
       [(char-numeric? first)
        (read-digits chars*)]
       [(char=? first #\-)
        (let ([after (cdr chars*)])
          (if (and (pair? after) (char-numeric? (car after)))
              (let ([res (read-digits after)])
                (cons (- (car res)) (cdr res)))
              #f))]
       [else #f])]))

(define (read-dollar chars)
  (define chars* (skip-space chars))
  (if (and (pair? chars*) (char=? (car chars*) #\$))
      (let ([res (read-digits (skip-space (cdr chars*)))])
        (if res res #f))
      #f))

(define (get-history hist id)
  (define rev (reverse hist))
  (if (and (integer? id)
           (>= id 1)
           (<= id (length rev)))
      (list-ref rev (- id 1))
      (error 'history "invalid id")))

;; ---------- Recursive Evaluator ----------
(define (eval-expr chars hist)
  (define chars* (skip-space chars))
  (cond
    [(null? chars*) (error 'parse "unexpected end of input")]
    [else
     (define first (car chars*))
     (cond
       [(or (char=? first #\+) (char=? first #\*) (char=? first #\/))
        (define v1+rest (eval-expr (cdr chars*) hist))
        (define v1 (car v1+rest))
        (define rest1 (cdr v1+rest))
        (define v2+rest (eval-expr rest1 hist))
        (define v2 (car v2+rest))
        (define rest2 (cdr v2+rest))
        (cond
          [(char=? first #\+) (cons (+ v1 v2) rest2)]
          [(char=? first #\*) (cons (* v1 v2) rest2)]
          [(char=? first #\/)
           (when (zero? v2) (error 'eval "divide by zero"))
           (cons (quotient v1 v2) rest2)])]

       [(char=? first #\-)
        (define res (read-int chars*))
        (if res
            res
            (let ([v+rest (eval-expr (cdr chars*) hist)])
              (cons (- (car v+rest)) (cdr v+rest))))]

       [(char=? first #\$)
        (define res (read-dollar chars*))
        (if res
            (cons (get-history hist (car res)) (cdr res))
            (error 'parse "bad $ format"))]

       [(char-numeric? first)
        (define res (read-int chars*))
        (if res res (error 'parse "bad number"))]

       [else (error 'parse "invalid token")])]))

(define (done? rest)
  (null? (skip-space rest)))

;; ---------- REPL Loop ----------
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
       (define chars (string->list line))
       (define result+rest (eval-expr chars hist))
       (define result (car result+rest))
       (define rest (cdr result+rest))
       (when (and rest (not (done? rest)))
         (error 'parse "extra input"))
       (define new-hist (cons result hist))
       (define id (length new-hist))
       (display id)
       (display ": ")
       (display (to-float result))
       (newline)
       (repl new-hist))]))

(repl '())


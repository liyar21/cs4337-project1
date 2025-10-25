#lang racket
(require "mode.rkt")

(define (to-float n) (real->double-flonum n))

(define (ws-skip cs)
  (cond [(null? cs) cs]
        [(char-whitespace? (car cs)) (ws-skip (cdr cs))]
        [else cs]))

(define (read-digits cs)
  (let loop ([c cs] [acc '()])
    (cond
      [(and (pair? c) (char-numeric? (car c)))
       (loop (cdr c) (cons (car c) acc))]
      [else
       (if (null? acc)
           #f
           (values (string->number (list->string (reverse acc))) c))])))

(define (read-int cs)
  (define cs* (ws-skip cs))
  (cond
    [(null? cs*) #f]
    [else
     (define c (car cs*))
     (cond
       [(char-numeric? c)
        (call-with-values (λ() (read-digits cs*)) (λ(n rest) (values n rest)))]
       [(char=? c #\-)
        (let ([after (cdr cs*)])
          (if (and (pair? after) (char-numeric? (car after)))
              (call-with-values (λ() (read-digits after))
                (λ(n rest) (values (- n) rest)))
              #f))]
       [else #f])]))

(define (read-dollar cs)
  (define cs* (ws-skip cs))
  (if (and (pair? cs*) (char=? (car cs*) #\$))
      (let ([r (read-digits (cdr cs*))])
        (and r (call-with-values (λ() r) (λ(n rest) (values n rest)))))
      #f))

(define (hist-get hist id)
  (let* ([rev (reverse hist)])
    (if (and (integer? id) (>= id 1) (<= id (length rev)))
        (list-ref rev (sub1 id))
        (error 'history "bad id"))))

;; v1: fixed unary '-' handling and $n robustness; still uses inexact `/`
(define (eval-expr cs hist)
  (define cs* (ws-skip cs))
  (cond
    [(null? cs*) (error 'parse "unexpected end")]
    [else
     (define c (car cs*))
     (cond
       [(or (char=? c #\+) (char=? c #\*) (char=? c #\/))
        (define-values (v1 r1) (eval-expr (cdr cs*) hist))
        (define-values (v2 r2) (eval-expr r1 hist))
        (cond
          [(char=? c #\+) (values (+ v1 v2) r2)]
          [(char=? c #\*) (values (* v1 v2) r2)]
          [(char=? c #\/) (values (/ v1 v2) r2)])] ; still wrong vs spec
       [(char=? c #\-)
        (define lit (read-int cs*))
        (if lit
            (call-with-values (λ() lit) (λ(n rest) (values n rest)))
            (let-values ([(v r)] (eval-expr (cdr cs*) hist))
              (values (- v) r)))]
       [(char=? c #\$)
        (define r (read-dollar cs*))
        (unless r (error 'parse "bad $"))
        (call-with-values (λ() r)
          (λ(n rest) (values (hist-get hist n) rest)))]
       [(char-numeric? c)
        (define r (read-int cs*))
        (if r
            (call-with-values (λ() r) (λ(n rest) (values n rest)))
            (error 'parse "bad number"))]
       [else (error 'parse "invalid token")])]))

(define (no-leftover? rest) (null? (ws-skip rest)))

(define (repl hist)
  (when prompt? (display "> "))
  (define line (read-line))
  (cond
    [(eof-object? line) (void)]
    [(string=? line "quit") (void)]
    [else
     (with-handlers ([exn:fail?
                      (λ(_)
                        (displayln "Error: Invalid Expression")
                        (repl hist))])
       (define chars (string->list line))
       (define-values (val rest) (eval-expr chars hist))
       (unless (no-leftover? rest) (error 'parse "extra"))
       (define new-hist (cons val hist))
       ;; v1: switched to this id calc so it matches “growing” ids
       (define id (length (reverse new-hist)))
       (display id) (display ": ") (display (to-float val)) (newline)
       (repl new-hist))]))

(repl '())


#lang racket

(require "mode.rkt")

;; ---------- Helpers ----------
(define (to-float n) (real->double-flonum n))

(define (skip-space chars)
  (cond
    ((null? chars) chars)
    ((char-whitespace? (car chars)) (skip-space (cdr chars)))
    (else chars)))

;; Reads digits; returns (cons number rest) or #f
(define (read-digits chars)
  (let loop ((c chars) (acc '()))
    (cond
      ((and (pair? c) (char-numeric? (car c)))
       (loop (cdr c) (cons (car c) acc)))
      (else
       (if (null? acc)
           #f
           (cons (string->number (list->string (reverse acc))) c))))))

;; Reads optional '-' + digits; skips leading space
(define (read-int chars)
  (define chars* (skip-space chars))
  (cond
    ((null? chars*) #f)
    (else
     (define first (car chars*))
     (cond
       ((char-numeric? first)
        (read-digits chars*))
       ((char=? first #\-)
        (let ((after (cdr chars*)))
          (if (and (pair? after) (char-numeric? (car after)))
              (let ((res (read-digits after)))
                (cons (- (car res)) (cdr res)))
              #f)))
       (else #f)))))

;; Reads $ then digits; allows spaces before '$' and after
(define (read-dollar chars)
  (define chars* (skip-space chars))
  (if (and (pair? chars*) (char=? (car chars*) #\$))
      (let ((res (read-digits (skip-space (cdr chars*)))))
        (if res res #f))
      #f))

;; History lookup by 1-based id (most recent has largest id)
(define (get-history hist id)
  (define rev (reverse hist))
  (if (and (integer? id)
           (>= id 1)
           (<= id (length rev)))
      (list-ref rev (- id 1))
      (error 'history "invalid id")))

(define (done? rest)
  (null? (skip-space rest)))

;; ---------- Parser/Evaluator ----------
;; Returns (cons value rest-chars)
(define (eval-expr chars hist)
  (define chars* (skip-space chars))
  (cond
    ((null? chars*) (error 'parse "unexpected end of input"))
    (else
     (define first (car chars*))
     (cond
       ;; + * /
       ((or (char=? first #\+) (char=? first #\*) (char=? first #\/))
        (define res1 (eval-expr (cdr chars*) hist))
        (define v1 (car res1))
        (define rest1 (cdr res1))
        (define res2 (eval-expr rest1 hist))
        (define v2 (car res2))
        (define rest2 (cdr res2))
        (cond
          ((char=? first #\+) (cons (+ v1 v2) rest2))
          ((char=? first #\*) (cons (* v1 v2) rest2))
          ((char=? first #\/)
           (when (zero? v2) (error 'eval "divide by zero")) 
           (cons (quotient v1 v2) rest2))))

       ;; '-' as negative literal or unary negation
       ((char=? first #\-)
        (define res (read-int chars*))
        (if res
            res
            (let ((res (eval-expr (cdr chars*) hist)))
              (cons (- (car res)) (cdr res)))))

       ;; $n history
       ((char=? first #\$)
        (define res (read-dollar chars*))
        (if res
            (cons (get-history hist (car res)) (cdr res))
            (error 'parse "bad $ format")))

       ;; number literal
       ((char-numeric? first)
        (define res (read-int chars*))
        (if res res (error 'parse "bad number")))

       (else (error 'parse "invalid token"))))))

;; ---------- REPL ----------
(define (repl hist)
  (when prompt? (display "> "))
  (define line (read-line))
  (cond
    ((eof-object? line) (void))
    ((string=? line "quit") (void))
    (else
     (define trimmed (string-trim line))
     (if (string=? trimmed "")
         (repl hist) ; skip blanks
         (with-handlers
             ((exn:fail?
               (λ (_e) ; why: keep error surface per spec
                 (displayln "Error: Invalid Expression")
                 (repl hist))))
           (define chars (string->list trimmed))
           (define res (eval-expr chars hist))
           (define val (car res))
           (define rest (cdr res))
           (when (and rest (not (done? rest)))
             (error 'parse "extra input"))
           (define new-hist (cons val hist))
           (define id (length new-hist))
           (display id) (display ": ")
           (display (to-float val)) (newline)
           (repl new-hist))))))

;; ---------- Start ----------
(repl '())


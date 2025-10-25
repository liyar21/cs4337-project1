#lang racket
;; Version 1 - Initial skeleton setup
;; Goal: create a simple calculator loop structure
;; (No parsing yet, just reads user input and echoes it)

(require "mode.rkt") ; defines prompt?

(define (repl hist)
  (when prompt? (display "> "))  ; show prompt only if interactive
  (define line (read-line))
  (cond
    [(eof-object? line) (void)]     ; quit on Ctrl+D / EOF
    [(string=? line "quit") (void)] ; quit command
    [else
     (displayln (string-append "You typed: " line))
     (repl hist)]))

;; start the calculator
(repl '())


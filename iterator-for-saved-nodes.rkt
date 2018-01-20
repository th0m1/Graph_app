#lang racket

(require "positioning-hash.rkt")
(provide save-positioning-to-file expr->str str->list-int take-positioning-from-file-by-line line->assoclist iterator)
(define liste (list 0 1 2 3 4 5 6))
(define rand (random-positioning-of-node-list 500 500 liste))
;---------------------------------------- Functions -----------------
(define iterator-capacity 5)
(port-count-lines-enabled #t) 
(define is-file-reversed-order #f)

(define (save-positioning-to-file positioning)
  (call-with-output-file "temp-saved-node-for-positioning.txt"
    (lambda (out)
      (newline)
      (write (hash->list positioning) out)
      (fprintf out "\n"))
    #:mode 'text
    #:exists 'append))

    
(define (expr->str input)
  (regexp-match* "[0-9]+" input))

(define (str->list-int input)
  (list (string->number (first input))
        (list (string->number (second input))
              (string->number (third input)))))

(define in (open-input-file "temp-saved-node-for-positioning.txt" #:mode 'text ))

(define (take-positioning-from-file-by-line in)
  (read-line in ))

(define (line->assoclist one-line-from-file) ;transform one line into assoclist , for overwriting positioning 
  (let/ec return
    (begin
      (define current-list '())
      (when (eof-object? one-line-from-file)
        (return ""))
      (define expr (regexp-match* "([0-9]+ [0-9]+ [0-9]+)" one-line-from-file)) ; '("6 383 75" "5 372 50" "4 213 429" "3 491 459")
      (for ([point (in-list expr)])
        (define pts (str->list-int (expr->str point)))
        (set! current-list (append current-list (list pts))))
      current-list)))

(define (iterator iterator-capacity)
  (define container (make-vector iterator-capacity))
  (let/ec return
    (begin
      (for ([i (in-range iterator-capacity)])
        (define line (take-positioning-from-file-by-line in))
        (if (eof-object? line)
            (return container)
            (vector-set! container i (line->assoclist line)))
        )
      (return container)
      )))
      
;(define iter (iterator iterator-capacity))




;(save-positioning-to-file rand)
;(define c (take-positioning-from-file-by-line in))
;(define expr (regexp-match* "([0-9]+ [0-9]+ [0-9]+)" c))
;(define str (expr->str (first expr)))
;(define pts (str->list-int str))
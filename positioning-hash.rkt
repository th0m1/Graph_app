#lang racket
(require "vect2D.rkt")

(provide apply-positioning positioning-of-assoc-list random-positioning-of-node-list positioning-move-node print-positioning)



(define (apply-positioning positioning node-id)
  (if (hash-ref positioning node-id void)  ; to handle the no node reference error when deleting a node
      (hash-ref positioning node-id void)
      void))

(define assoclist (list (list 1 (list 1 1)) (list 2 (list 2 2)) (list 3 (list 3 3))))

(define (positioning-of-assoc-list L )
  (define positioning (make-hash))
  (define (iter L)
    (if (empty? L)
        positioning
        (begin (hash-set! positioning (car (car L)) (cadr (car L)))
               (iter (cdr L)))))
  (iter L))


(define (random-positioning-of-node-list w h L)
  (define positioning (make-hash))
  (define (iter L)
    (if (empty? L)
        positioning
        (begin (hash-set! positioning (car L) (list (random 1 w) (random 1 h)))
               (iter (cdr L)))))
  (iter L))


(define (positioning-move-node positioning node-id vect)
  (if (hash-has-key? positioning node-id)
      (hash-set! positioning node-id (list (+ (car (hash-ref positioning node-id)) (car vect))
                                           (+ (cadr (hash-ref positioning node-id)) (cadr vect))))
      "key not found"))

#;(positioning-move-node ttt 1 (list 10000 100000))

(define (print-positioning id-list positioning)
  (for ([i (in-list id-list)])
    (display (format " ~a ~a\n" i (hash-ref positioning i)))))

#;(print-positioning (list 1 2 3 4 5) rand)







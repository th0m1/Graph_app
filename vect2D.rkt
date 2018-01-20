#lang racket


;Module du vecteur 2D , la liste se transforme en points

(provide make-vect coord-x coord-y vect-sum vect-sum* scalar vect-norm vect-unit vect-distance)


(define (make-vect x y)
  (list x y))

(define (coord-x v)
  (if (void? v)
      0
      (car v)))

(define (coord-y v)
  (if (void? v)
      0
      (cadr v)))

(define (vect-sum v1 v2)
  (make-vect (+ (coord-x v1) (coord-x v2)) (+ (coord-y v1) (coord-y v2))))

(define (filtrer-x List)
  (if (empty? List)
      empty
      (cons (coord-x (car List)) (filtrer-x (cdr List)))))

(define (filtrer-y List)
  (if (empty? List)
      empty
      (cons (coord-y (car List)) (filtrer-y (cdr List)))))

(define (vect-sum* List)
  (make-vect (apply + (filtrer-x List)) (apply + (filtrer-y List))))
  
      

; these vector are used for testing (define List (list (list 1 2) (list 1 2) (list 1 2) (list 1 2)))

(define (scalar k v)
  (make-vect (* k (coord-x v)) (* k (coord-y v))))

(define (vect-norm v)
  (sqrt (+ (sqr (coord-x v)) (sqr (coord-y v)))))

(define (vect-unit v)
  (let ((cst (vect-norm v)))
    (list (/ (coord-x v) cst) (/ (coord-y v) cst))))

(define (vect-distance v1 v2)   ; for computing the translation in the canvas 
  (make-vect (- (coord-x v2) (coord-x v1)) (- (coord-y v2) (coord-y v1))))

  

  




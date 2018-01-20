#lang racket

(require "../vect2D.rkt")


(define V-test-1 (make-vect 1 100))
(format "make-vect return : ~a" V-test-1)
(format "x coordinate of V-test-1 is ~a and y coordinate of V-test-1 is ~a" (coord-x V-test-1) (coord-y V-test-1))
(define V-test-2 (make-vect 100 1))
(format "vect-sum takes 2 vector and return their sum , vect-sum return ~a" (vect-sum V-test-1 V-test-2))

(define V-test-L (list V-test-1 V-test-2 (list 200 200) (list 300 300)))

(format "vect-sum* takes a list of vectors and return their sum , ~a" (vect-sum* V-test-L))
(format "vect-scalar muliply the vector V-test-1 by k = 10 ,~a" (scalar 10 V-test-1))
(format "(vect-norm V-test-1) : ~a" (vect-norm V-test-1))
(format "(vect-unit V-test-1) : ~a" (vect-unit V-test-1))



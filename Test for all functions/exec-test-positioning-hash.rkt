#lang racket

(require "../positioning-hash.rkt")


(define assoclist (list (list 1 (list 1 1)) (list 2 (list 2 2)) (list 3 (list 3 3))))
(format "(positioning-of-assoc-list assoclist) : ~a" (positioning-of-assoc-list assoclist))
(positioning-of-assoc-list assoclist)
(define testhash (positioning-of-assoc-list assoclist))
(format "(apply-positioning testhash 1) : ~a" (apply-positioning testhash 1))
(printf "\n")
(define liste (list 0 1 2 3 4 5 6))
(format "liste =  ~a " liste)
(define rand (random-positioning-of-node-list 500 500 liste))
(format "(random-positioning-of-node-list 500 500 liste) : ~a" rand)
(printf "\n")
(positioning-move-node testhash 1 (list 100000 100000))
(format "testhash := [ ~a ]  " testhash)
(format "(positioning-move-node testhash 1 (list 100000 100000)) : ~a" testhash)
(printf "(print-positioning (list 1 2 3 4 5) rand) \n")
(print-positioning (list 1 2 3 4 5) rand)





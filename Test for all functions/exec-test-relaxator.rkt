#lang racket

(require "../relaxator.rkt")
(require "../positioning-hash.rkt")
(require "../hash.rkt")



(define liste (list 1 2 3 4))
(define test-graph2 (empty-graph))
(add-node! test-graph2 1)
(add-node! test-graph2 2)
(add-node! test-graph2 3)
(add-node! test-graph2 4)
(add-edge! test-graph2 1 2)
(add-edge! test-graph2 2 3)
(add-edge! test-graph2 3 4)
(define rand (random-positioning-of-node-list 500 500 liste))
(format "initial positions ~a" rand)
(define my-relaxator (new-relaxator))
(my-relaxator 'relax test-graph2 rand)
(format "my relax method works in-place")
(format "the next positions for positioning become [ ~a ]in a chain graph structure " rand)
(printf "\n")
(format " (get-c1) := ~a" (my-relaxator 'get-c1))
(format " (get-c2) := ~a" (my-relaxator 'get-c2))
(format " (get-c4) := ~a" (my-relaxator 'get-c4))
(printf "\n")

(my-relaxator 'set-c1 10)
(my-relaxator 'set-c2 80)
(my-relaxator 'set-c4 60)

(format " (set-c1) := ~a" (my-relaxator 'get-c1))
(format " (set-c2) := ~a" (my-relaxator 'get-c2))
(format " (set-c4) := ~a" (my-relaxator 'get-c4))
(printf "\n")

(format " (relax-c1) := ~a" (my-relaxator 'relax-c1))
(format " (relax-c2) := ~a" (my-relaxator 'relax-c2))
(format " (relax-c3) := ~a" (my-relaxator 'relax-c3))
(format " (relax-c4) := ~a" (my-relaxator 'relax-c4))







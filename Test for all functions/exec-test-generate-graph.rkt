#lang racket

(require "../generate-graph.rkt")
(require "../positioning-hash.rkt")
(require "../relaxator.rkt")

(define chain (chain-graph 10))
(define cyclic (cyclic-graph 10))
(define tree (complete-tree-graph 3 2))
(define grids (grid 5 3))
(define clic (clic-graph 10))



(format "(chain-graph 10) := ~a " chain)
(printf "\n")
(format "(cyclic-graph 10) := ~a " cyclic)
(printf "\n")
(format "(complete-tree-graph 3 2) := ~a " tree)
(printf "\n")
(format "(grid 5 3) := ~a " grids)
(printf "\n")
(format "(clic-graph 10) := ~a " clic)
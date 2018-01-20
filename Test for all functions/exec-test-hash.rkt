#lang racket

(require "../hash.rkt")

(format "(empty-graph) : ~a" (empty-graph))
(define test-graph (empty-graph))
(add-node! test-graph 1)

(format "(add-node! test-graph 1) : ~a" test-graph)
(add-node! test-graph 2)
(add-node! test-graph 3)
(add-edge! test-graph 2 3)
(format "(add-edge! test-graph 2 3) : ~a" test-graph)
(format "(add-edge! test-graph 2 3) : ~a" (add-edge! test-graph 2 3))
(format "(add-edge! test-graph 2 4) : ~a" (add-edge! test-graph 2 4))
(format "(add-edge! (empty-graph) 2 4) : ~a" (add-edge! (empty-graph) 2 4))

(printf "\n")
(format "(get-nodes test-graph) : ~a" (get-nodes test-graph))
(format "(get-neighbors test-graph 3) : ~a" (get-neighbors test-graph 3))
(printf "\n")

(format "(rm-node! test-graph 3) : ~a" (rm-node! test-graph 3))
(format "test-graph : ~a" test-graph)
(format "(rm-node! test-graph 3) : ~a" (rm-node! test-graph 3))
(printf "\n")
(add-edge! test-graph 1 2)
(format "(add-edge! test-graph 1 2) : ~a" test-graph)
(rm-edge! test-graph 1 2)
(format "(rm-edge! test-graph 1 2) : ~a" test-graph)
(format "(rm-edge! test-graph 1 2) : ~a" (rm-edge! test-graph 1 2))
(printf "\n")

(define test-graph2 (empty-graph))
(add-node! test-graph2 1)
(add-node! test-graph2 2)
(add-node! test-graph2 3)
(add-node! test-graph2 4)
(format "(print-graph test-graph2) : ~a" (print-graph test-graph2))












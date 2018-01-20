#lang racket


(require "vect2D.rkt")
(require "positioning-hash.rkt")
(require "hash.rkt")
(require "relaxator.rkt")
(require data/queue)

(provide chain-graph cyclic-graph complete-tree-graph grid clic-graph calculate-nodes-tree)
;---------------------------------------------->

(define (chain-graph n)
  (define graph (empty-graph))
  (for ([i (in-range n)])
    (add-node! graph i))
  (define nextNode 1)
  (for ([i (in-range (- n 1))])
    (add-edge! graph i nextNode)
    (set! nextNode (+ 1 nextNode)))
  graph)

(define chain (chain-graph 10))

;----------------------------------------------->
(define (cyclic-graph n)
  (define graph (chain-graph n))
  (add-edge! graph 0 (- n 1))
  graph)

(define cyclic (cyclic-graph 10))


;----------------------------------------------->
; Breath first search traversal style using a queue to contruct the k-ary for the graph 
(define (complete-tree-graph arity depth)
  (define graph (empty-graph))
  (define Node 1)
  (define Parent 0)
  (add-node! graph 0)
  (define queueNode (make-queue))
  (for ([i (in-range 1 (+ arity 1))])
    (add-node! graph Node)
    (add-edge! graph 0 Node)
    (set! Node (+ 1 Node))
    (enqueue! queueNode i))
  (set! Parent (+ 1 Parent))
  (for ([i (in-range (- depth 1))])
    (for ([Nodes (in-list (queue->list queueNode))])
      (for ([times (in-range arity)])
        (add-node! graph Node)
        (add-edge! graph Nodes Node)
        (set! Node (+ 1 Node))
        (enqueue! queueNode Node))
      (dequeue! queueNode)))
    graph)

(define (calculate-nodes-tree arity depth)
  (/ (- (expt arity (+ depth 1)) 1) (- arity 1)))


(define tree (complete-tree-graph 3 2))

;------------------------------------------------------------->

(define (grid n m)
  (define graph (chain-graph (* n m)))
  (define edge2remove (- n 1))
  (for ([i (in-range (- m 1))])
    (rm-edge! graph edge2remove (+ 1 edge2remove))
    (set! edge2remove (+ edge2remove n)))
  ;(define verticalTraversalNode 0)   ;[verticalTraversalNode currentnode]
  ;(define verticalTraversalNextNode n) ;;[verticalTraversalNextNode (+ (+ 1 currentnode) n))]
  (for ([currentnode (in-range n)])
  (define verticalTraversalNode currentnode)
  (define verticalTraversalNextNode (+  currentnode n))
    (for ([j (in-range (- m 1))])
      (add-edge! graph verticalTraversalNode verticalTraversalNextNode)
      (set! verticalTraversalNextNode (+ verticalTraversalNextNode n))
      (set! verticalTraversalNode (+ verticalTraversalNode n)))
    ;(set! verticalTraversalNode currentnode)
    ;(set! verticalTraversalNextNode (+ (+ 1 currentnode) n)))
    )
    graph)

;-------------------------------------------------------------->


(define (clic-graph n)
  (define graph (empty-graph))
  (for ([node (in-range n)])
    (add-node! graph node))
  (for ([node (in-range n)])
    (for ([neighbors (in-range node n)])
      (add-edge! graph node neighbors)))
  (for ([node (in-range n)])
    (rm-edge! graph node node))
  graph)






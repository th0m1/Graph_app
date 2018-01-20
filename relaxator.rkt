#lang racket

(require "vect2D.rkt")
(require "positioning-hash.rkt")
(require "hash.rkt")

(provide new-relaxator)

(define (new-relaxator)
  (define c1 2)
  (define c2 15)
  (define c3 (sqr c2))
  (define c4 2)
  
  (define banned-nodes (mutable-set))
  
  (define (ban-node! node ban-set)
    (set-add! banned-nodes node))

  (define (clear-banned-set banned-nodes)
    (set-clear! banned-nodes))
  
  (define (deban-node! node ban-set)
    (when (set-member? ban-set node)
        (set-remove! ban-set node)))

  (define (delete-from-graph node graph)
    (rm-node! graph node))
  
  (define (force-m positioning node-id1 node-id2)
    (define vect-id1 (scalar -1 (apply-positioning positioning node-id1)))
    (define vect-id2 (apply-positioning positioning node-id2))
    (define dist (vect-sum vect-id1 vect-id2))
    (define norm (vect-norm dist))
    (define calc (scalar (* c1 (log (/ norm c2))) (vect-unit dist)))
    calc)
  
  (define (force-e positioning node-id1 node-id2)
    (define vect-id1 (scalar -1 (apply-positioning positioning node-id1)))
    (define vect-id2 (apply-positioning positioning node-id2))
    (define dist (vect-sum vect-id1 vect-id2))
    (define norm (vect-norm dist))
    (define calc (scalar (- (/ c3 (sqr norm))) (vect-unit dist)))
    calc)
  
  (define (relax graph rand)
    (define dx (make-vect 0 0))
    (for ([i (in-list (get-nodes graph))])
      (when (not (set-empty? (get-neighbors graph i)))
        (for ([j (in-list (set->list (get-neighbors graph i)))])
        (when (and (not (set-member? banned-nodes i)) (not (set-member? banned-nodes j)))
          (define vect-m (force-m rand i j))
          (set! dx (vect-sum dx vect-m)))))
      (define node-self (list i) ) 
      (for ([k (get-nodes graph)])
        (when (and (not (member k node-self)) (not (set-member? banned-nodes k)))
          (define vect-e (force-e rand i k))
          (set! dx (vect-sum dx vect-e))))
      #;(display dx)
      (positioning-move-node rand i (scalar c4 dx))
      (set! dx (make-vect 0 0))))
  
  (define (this method . Largs)
    (case method
      ((relax-c1) c1)
      ((relax-c2) c2)
      ((relax-c3) c3)
      ((relax-c4) c4)
      ((relax) (relax (car Largs) (cadr Largs)))
      ((set-c1!) (lambda () (set! c1 (car Largs))))
      ((set-c2!) (lambda () (set! c2 (car Largs))))
      ((set-c4!) (lambda () (set! c4 (car Largs))))
      ((get-c1) c1)
      ((get-c2) c2)
      ((get-c4) c4)
      ((ban-node!) (ban-node! (car Largs) banned-nodes))
      ((deban-node!) (deban-node! (car Largs) banned-nodes))
      ((delete-node) (delete-from-graph (car Largs) (cadr Largs)))
      ((clear-banned-set) (clear-banned-set banned-nodes))
      (else c1)))
  this)















  
  



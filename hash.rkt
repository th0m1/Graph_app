#lang racket


(provide empty-graph add-node! add-edge! get-nodes get-neighbors rm-node! rm-edge! print-graph)

(define (empty-graph)
  (make-hash))

(define (add-node! graph node-id)
  (if (hash-has-key? graph node-id)
      graph
      (hash-set! graph node-id (set))))

(define (add-edge! graph node-id1 node-id2)
  (cond [(hash-empty? graph) "the graph is empty"]
        [(not (hash-has-key? graph node-id1)) (format "node id ~a is not in the graph" node-id1)]
        [(not (hash-has-key? graph node-id2)) (format "node id ~a is not in the graph" node-id2)]
        [(not (set-member? (hash-ref graph node-id1) node-id2))
         (begin (hash-set! graph node-id1 (set-add (hash-ref graph node-id1) node-id2))
                (if (not (set-member? (hash-ref graph node-id2) node-id1))
                    (hash-set! graph node-id2 (set-add (hash-ref graph node-id2) node-id1))
                    graph))]
        [(not (set-member? (hash-ref graph node-id2) node-id1))
         (begin (hash-set! graph node-id2 (set-add (hash-ref graph node-id2) node-id1))
                (if (not (set-member? (hash-ref graph node-id1) node-id2))
                    (hash-set! graph node-id1 (set-add (hash-ref graph node-id1) node-id2))
                    graph))]
        [else (format "nodes ~a and ~a are already neighbors" node-id1 node-id2)]))

(define (get-nodes graph)
  (hash-keys graph))


(define (get-neighbors graph node-id)
  (hash-ref graph node-id (set)))


(define (rm-node! graph node-id)
  (cond [(not (hash-has-key? graph node-id)) (format "node ~a is not in the graph" node-id)]
        [else (begin (hash-remove! graph node-id)
                     (for/list ([i (get-nodes graph)])
                       (if (set-member? (get-neighbors graph i) node-id)
                           (hash-set! graph i (set-remove (get-neighbors graph i) node-id))
                           graph)))]))

(define (rm-edge! graph node-id1 node-id2)
  (cond [(set-member? (get-neighbors graph node-id1) node-id2)
         (begin (hash-set! graph node-id1 (set-remove (get-neighbors graph node-id1) node-id2))
                (if (set-member? (get-neighbors graph node-id2) node-id1)
                    (hash-set! graph node-id2 (set-remove (get-neighbors graph node-id2) node-id1))
                    graph))]
        [(set-member? (get-neighbors graph node-id2) node-id1)
         (begin (hash-set! graph node-id2 (set-remove (get-neighbors graph node-id2) node-id1))
                (if (set-member? (get-neighbors graph node-id1) node-id2)
                    (hash-set! graph node-id1 (set-remove (get-neighbors graph node-id1) node-id2))
                    graph))]
        [else "no edge"]))

(define (print-graph graph)
  (hash-values graph))

  

         
         
  


                    

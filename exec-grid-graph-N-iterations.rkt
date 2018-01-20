#lang racket


(require "vect2D.rkt")
(require "positioning-hash.rkt")
(require "hash.rkt")
(require "relaxator.rkt")
(require "generate-graph.rkt")



(define N-iterations 100)
(define chain2 (chain-graph 5))
(define pts (random-positioning-of-node-list 20 20 (list 0 1 2 3 4)))
(format "initial positions ~a of the chain graph ~a" pts chain2)
(define my-relaxator (new-relaxator))
 (for ([i (in-range N-iterations)])
  (my-relaxator 'relax chain2 pts))
(printf "\n")
(format "(my-relaxator 'relax chain2 pts) after ~a iterations " N-iterations)
(format "~a " pts)
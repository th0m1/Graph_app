#lang racket/gui

(require "vect2D.rkt")
(require "positioning-hash.rkt")
(require "hash.rkt")
(require "relaxator.rkt")
(require "generate-graph.rkt")
(require "iterator-for-saved-nodes.rkt")

(define-syntax while
  (syntax-rules ()
    ((while pred? stmt ...)
     (do () ((not pred?))
       stmt ...))))
(define SIZE 700)

(define FRAME
  (new frame%
       (label "Graph visualization")
       (width 700)
       (height 800)
       (border 10)
       #;(spacing 10)
       (stretchable-width #f)
       (stretchable-height #f)))


(define VPANEL 
  (new vertical-panel%  (parent FRAME)))
(define horizVPANEL
  (new horizontal-panel% (parent VPANEL)))
(define pause (new button%
                   (parent horizVPANEL)
                   ;(style (list 'border))
                   (label (read-bitmap "./icon/pause.png"))
                   (callback (lambda (b evt)
                               (send CANVAS suspend-flush)
                               (set! end #t) (send CANVAS on-paint)))))
(define resume (new button%
                    (parent horizVPANEL)
                    ;(style (list 'border))
                    (label (read-bitmap "./icon/resume.png"))
                    (callback (lambda (b evt)
                                (send CANVAS resume-flush)
                                (set! end #f)
                                (send CANVAS on-paint)))))
(define new-graph (new button%
                       (parent horizVPANEL)
                       ;(style (list 'border))
                       (label (read-bitmap "./icon/new-graph.png"))
                       (callback (lambda (b evt)
                                   (define my-graph (generate-random))
                                   (define tmp (my-graph 'grid 10 10))
                                   (my-relaxator 'clear-banned-set )
                                   (set! graph (car tmp))
                                   (set! saved-graph (hash-copy graph))
                                   (set! positioning (cadr tmp))
                                   (set! saved-positioning (hash-copy positioning))
                                   ))))

(define restart (new button%
                     (parent horizVPANEL)
                     ;(style (list 'border))
                     (label (read-bitmap "./icon/restart.png"))
                     (callback (lambda (b evt)
                                 (my-relaxator 'clear-banned-set )
                                 (set! graph (hash-copy saved-graph))
                                 (set! positioning (hash-copy saved-positioning))))))

(define shake (new button%
                   (parent horizVPANEL)
                   ;(style (list 'border))
                   (label (read-bitmap "./icon/shake.png"))
                   (callback (lambda (b evt)
                               (for ([node (in-list (get-nodes graph))])
                                 (positioning-move-node positioning node (make-vect (random -15 15) (random -15 15))))
                               #;(send CANVAS refresh-now)))))

(define center (new button%
                    (parent horizVPANEL)
                    ;(style (list 'border))
                    (label (read-bitmap "./icon/center.png"))
                    (callback (lambda (b evt)
                                (send bitmap-dc set-transformation (vector (vector-ref (send bitmap-dc get-transformation) 0) 0.0 0.0 1.0 1.0 0))))))

(define zoom (new button%
                  (parent horizVPANEL)
                  ;(style (list 'border))
                  (label (read-bitmap "./icon/zoom.png"))
                  (callback (lambda (b evt)
                              (send bitmap-dc set-transformation (vector (vector-ref (send bitmap-dc get-transformation) 0)
                                                                         (- (- (* (* 1.2 (vector-ref (send bitmap-dc get-transformation) 3)) (/ SIZE 2)) (/ SIZE 2)))
                                                                         (- (- (* (* 1.2 (vector-ref (send bitmap-dc get-transformation) 4)) (/ SIZE 2)) (/ SIZE 2)))
                                                                         (* 1.2 (vector-ref (send bitmap-dc get-transformation) 3)) 
                                                                         (* 1.2 (vector-ref (send bitmap-dc get-transformation) 4)) 0))))))

(define dezoom (new button%
                    (parent horizVPANEL)
                    ;(style (list 'border))
                    (label (read-bitmap "./icon/dezoom.png"))
                    (callback (lambda (b evt)
                                (send bitmap-dc set-transformation (vector (vector-ref (send bitmap-dc get-transformation) 0)
                                                                           (- (- (* (* 0.9 (vector-ref (send bitmap-dc get-transformation) 3)) (/ SIZE 2)) (/ SIZE 2)))
                                                                           (- (- (* (* 0.9 (vector-ref (send bitmap-dc get-transformation) 3)) (/ SIZE 2)) (/ SIZE 2)))
                                                                           (* 0.9 (vector-ref (send bitmap-dc get-transformation) 3)) 
                                                                           (* 0.9 (vector-ref (send bitmap-dc get-transformation) 4)) 0))
                                #;(set! epsilon (* 0.9 epsilon))))))

(define unfreeze (new button%
                      (parent horizVPANEL)
                      ;(style (list 'border))
                      (label (read-bitmap "./icon/unfreeze.png"))
                      (callback (lambda (b evt)
                                  (my-relaxator 'clear-banned-set)))))

(define add-random-node (new button%
                             (parent horizVPANEL)
                             ;(style (list 'border))
                             (label (read-bitmap "./icon/add-random.png"))
                             (callback (lambda (b evt)
                                         (define next-node-id (+ (vector-argmax (lambda (x) x) (list->vector (hash-keys positioning))) 1))
                                         (hash-set! positioning next-node-id (make-vect (random 2 SIZE) (random 2 SIZE)))
                                         (add-node! graph next-node-id)
                                         (define node-id-of-neighbor (vector-ref (list->vector (hash-keys positioning)) (random 0 (length (hash-keys positioning)))))
                                         (add-edge! graph next-node-id node-id-of-neighbor ) 
                                         ; if we got more times , we would implement add proportionnal nodes for each graph 
                                         ))))

(define tools (new button%
                   (parent horizVPANEL)
                   ;(style (list 'border))
                   (label (read-bitmap "./icon/tools.png"))
                   (callback (lambda (b evt)
                               (send dialog show-without-yield)))))

(define dialog (new dialog%
                    [label "Configuration"]
                    [parent FRAME]
                    [min-width 100]
                    [min-height 100]
                    [stretchable-width #f]
                    [stretchable-height #f]
                    [enabled #t]
                    ))

(define choice (new choice%
                    (label "Type of graph :")
                    (parent dialog)
                    (choices (list "line"
                                   "circle"
                                   "tree"
                                   "grid"
                                   "clic"))
                    (callback
                     (lambda (r evt)
                       (define selected (send r  get-string-selection))
                       (case selected
                         (("line") (begin (send dialog add-child setting-line)))
                         (("circle") (begin (send dialog add-child setting-circle)))
                         (("tree") (begin (send dialog add-child setting-tree)))
                         (("grid") (begin (send dialog add-child setting-grid)))
                         (("clic") (begin (send dialog add-child setting-clic)))
                         )))))
(define setting-line (new group-box-panel%
                          (parent dialog)
                          (label "Chain graph")
                          (style (list 'deleted))))

(define chains-number-of-nodes (new slider%
                                    (label "numbers of nodes")
                                    (parent setting-line)
                                    (min-value 1)
                                    (max-value 120)
                                    (init-value 40)
                                    ))

(define apply-line (new button%
                        (parent setting-line)
                        (label "Apply line!")
                        (callback (lambda (b evt)
                                    (define tmp (my-graph 'chain (send chains-number-of-nodes get-value)))
                                    (my-relaxator 'clear-banned-set )
                                    (set! graph (car tmp))
                                    (set! saved-graph (hash-copy graph))
                                    (set! positioning (cadr tmp))
                                    (set! saved-positioning (hash-copy positioning))))))

(define setting-circle (new group-box-panel%
                            (parent dialog)
                            (label "Circle graph")
                            (style (list 'deleted))))

(define circle-number-of-nodes (new slider%
                                    (label "numbers of nodes")
                                    (parent setting-circle)
                                    (min-value 1)
                                    (max-value 120)
                                    (init-value 40)
                                    ))

(define apply-circle (new button%
                          (parent setting-circle)
                          (label "Apply circle!")
                          (callback (lambda (b evt)
                                      (define tmp (my-graph 'cyclic (send circle-number-of-nodes get-value)))
                                      (my-relaxator 'clear-banned-set )
                                      (set! graph (car tmp))
                                      (set! positioning (cadr tmp))))))

(define setting-tree (new group-box-panel%
                          (parent dialog)
                          (label "Tree graph")
                          (style (list 'deleted))))

(define setting-tree-arity (new slider%
                                (label "arity ")
                                (parent setting-tree)
                                (min-value 2)
                                (max-value 5)
                                (init-value 2)
                                ))
(define setting-tree-depth (new slider%
                                (label "Depth")
                                (parent setting-tree)
                                (min-value 2)
                                (max-value 8)
                                (init-value 2)
                                ))


(define apply-tree (new button%
                        (parent setting-tree)
                        (label "Apply tree!")
                        (callback (lambda (b evt)
                                    (define tmp (my-graph 'tree (send setting-tree-arity get-value) (send setting-tree-depth get-value)))
                                    (my-relaxator 'clear-banned-set )
                                    (set! graph (car tmp))
                                    (set! saved-graph (hash-copy graph))
                                    (set! positioning (cadr tmp))
                                    (set! saved-positioning (hash-copy positioning))))))

(define setting-grid (new group-box-panel%
                          (parent dialog)
                          (label "Grid graph")
                          (style (list 'deleted))))


(define setting-grid-width (new slider%
                                (label "width ")
                                (parent setting-grid)
                                (min-value 2)
                                (max-value 15)
                                (init-value 5)
                                ))
(define setting-grid-height (new slider%
                                 (label "height")
                                 (parent setting-grid)
                                 (min-value 2)
                                 (max-value 15)
                                 (init-value 5)
                                 ))


(define apply-grid (new button%
                        (parent setting-grid)
                        (label "Apply grid!")
                        (callback (lambda (b evt)
                                    (define tmp (my-graph 'grid (send setting-grid-width get-value) (send setting-grid-height get-value)))
                                    (my-relaxator 'clear-banned-set )
                                    (set! graph (car tmp))
                                    (set! saved-graph (hash-copy graph))
                                    (set! positioning (cadr tmp))
                                    (set! saved-positioning (hash-copy positioning))))))


(define setting-clic (new group-box-panel%
                          (parent dialog)
                          (label "Clic graph")
                          (style (list 'deleted))))


(define setting-clic-number-of-nodes (new slider%
                                          (label "numbers of nodes ")
                                          (parent setting-clic)
                                          (min-value 2)
                                          (max-value 10)
                                          (init-value 5)
                                          ))


(define apply-clic (new button%
                        (parent setting-clic)
                        (label "Apply clic!")
                        (callback (lambda (b evt)
                                    (define tmp (my-graph 'clic (send setting-clic-number-of-nodes get-value) ))
                                    (my-relaxator 'clear-banned-set )
                                    (set! graph (car tmp))
                                    (set! saved-graph (hash-copy graph))
                                    (set! positioning (cadr tmp))
                                    (set! saved-positioning (hash-copy positioning))))))

                          


(define setting (new group-box-panel%
                     (parent dialog)
                     (label "Setting")
                     #;(style (list 'deleted))))

(define change-c2 (new slider%
                       (label "c2")
                       (parent setting)
                       (min-value 5)
                       (max-value 80)
                       (init-value 40)
                       ))

(define change-c4 (new slider%
                       (label "c4")
                       (parent setting)
                       (min-value 2)
                       (max-value 20)
                       (init-value 2)
                       ))
(define apply-setting (new button%
                           (parent setting)
                           (label "Apply !")
                           (callback (lambda (b evt)
                                       (define new-c2 (send change-c2 get-value))
                                       (define new-c4 (send change-c4 get-value))
                                       (my-relaxator 'set-c2! new-c2)
                                       (my-relaxator 'set-c4! new-c4)))))




(define HPANEL
  (new horizontal-panel% (parent VPANEL)))
(define pen-for-node (make-object pen% "red" 12 'solid 'round 'round))
(define pen-for-edge (make-object pen% "blue" 1.5 'solid 'round 'round))
(define bitmap (make-object bitmap% SIZE SIZE))
(define bitmap-dc (new bitmap-dc% (bitmap bitmap)))
(define end #f)

(define V #f); le point courant pour la souris
(define epsilon 10)
(define is-middle-pressed #f)

(define (proche? x y liste epsilon)
  (let/ec return
    ((begin (for ([(key value) (in-hash liste)])
              (define xi (coord-x value))
              (define yi (coord-y value))
              (when (< (+ (abs (- x xi)) (abs (- y yi))) epsilon)
                (return key)))
            (return #f)))))

(define vect2translate (make-vect 0 0))

(define MY-CANVAS%                  ; avec gestion de la souris
  (class canvas%
    
    (define/override (on-char event)
      (let ((key (send event get-key-code)))
        (define-values [point _] (get-current-mouse-state))
        (send point get-x)
        (send point get-y)
        (define x (send point get-x))
        (define y (send point get-y))
        (case key
          ['wheel-up    (set! value 1.2)]
          ['wheel-down  (set! value 0.9)])
        (send bitmap-dc set-transformation (vector (vector-ref (send bitmap-dc get-transformation) 0)
                                                   (- (- (* (* value (vector-ref (send bitmap-dc get-transformation) 3)) (/ SIZE 2)) (/ SIZE 2)))
                                                   (- (- (* (* value (vector-ref (send bitmap-dc get-transformation) 4)) (/ SIZE 2)) (/ SIZE 2)))
                                                   (* value (vector-ref (send bitmap-dc get-transformation) 3))
                                                   (* value (vector-ref (send bitmap-dc get-transformation) 4)) 0))
              )
          )
    

    
  (define/override (on-event evt)
    (case (send evt get-event-type) 
      ((left-down) (define x (send evt get-x)) 
                   (define y (send evt get-y))
                   (set! V (proche? x y positioning epsilon))
                   (when V
                     (my-relaxator 'ban-node! V)))
      ((left-up) (if V
                     (begin #;(my-relaxator 'deban-node! V) ; not releasing them because i made a button unfreeze
                            (set! V #f))
                     (set! V #f)))
      ((right-up) (if V
                      (begin (my-relaxator 'deban-node! V)
                             (set! V #f))
                      (set! V #f)))
      ((right-down) (define x (send evt get-x))
                    (define y (send evt get-y))
                    (define to-delete (proche? x y positioning epsilon))
                    (when to-delete
                      (my-relaxator 'ban-node! V)
                      (rm-node! graph (proche? x y positioning epsilon))
                      (hash-remove! positioning (proche? x y positioning epsilon))
                      ))
      ((middle-down)
       (define x (send evt get-x))
       (define y (send evt get-y))
       (set! is-middle-pressed #t)
       (set! vect2translate (make-vect x y))
       (when V
         (my-relaxator (my-relaxator 'clear-banned-set))
         (hash-set! positioning V (list (send evt get-x) (send evt get-y)))
         ))
      ((middle-up)
       (set! is-middle-pressed #f))
         
      (else (when V
              (hash-set! positioning V (list (send evt get-x) (send evt get-y))))
            (when is-middle-pressed
              (define vect (make-vect (send evt get-x) (send evt get-y)))
              (define translated (vect-distance (scalar 1/200 vect2translate) (scalar 1/200 vect)))
              (for ([pos (in-hash-keys positioning)])
                (positioning-move-node positioning pos translated))
              )
            )))
  (super-new)))

(define CANVAS
  (new MY-CANVAS%
       (parent HPANEL)
       (style '(border))
       (min-width (+ 0 SIZE))
       (min-height SIZE)
       (stretchable-width #f)
       (stretchable-height #f)
       (paint-callback (lambda (obj dc) (thread (lambda ()
                                                  (define dc (send CANVAS get-dc))
                                                  (while (not end)
                                                         (send bitmap-dc clear)
                                                         (send bitmap-dc set-pen pen-for-edge)
                                                         (for ([node (in-list (get-nodes graph))])
                                                           (for ([neighbors (in-list (set->list (get-neighbors graph node)))])
                                                             (send bitmap-dc draw-line (coord-x (apply-positioning positioning node))
                                                                   (coord-y (apply-positioning positioning node))
                                                                   (coord-x (apply-positioning positioning neighbors))
                                                                   (coord-y (apply-positioning positioning neighbors)))))
                                                         (send bitmap-dc set-pen pen-for-node)
                                                         (for ([point (in-list (hash-values positioning))])
                                                           (send bitmap-dc draw-point (coord-x point) (coord-y point)))
                                                         (send bitmap-dc set-smoothing 'smoothed)
                                                         (send dc draw-bitmap bitmap 0 0 'opaque)
                                                         #;(save-positioning-to-file positioning) ;saving all the points to file , the file become 2.9Mo for nearly a minute , it would be hard to catch all the points
                                                         (my-relaxator 'relax graph positioning)
                                                         (sleep 0.01))))))))


(define (draw-points pos)
  (define dc (send CANVAS get-dc))
  (send dc set-pen pen-for-node)
  (for ([point (in-list (hash-values pos))])
    (send dc draw-point (coord-x point) (coord-y point))))

(define (draw-edge graph pos)
  (define dc (send CANVAS get-dc))
  (send dc set-pen pen-for-edge)
  (for ([node (in-list (get-nodes graph))])
    (for ([neighbors (in-list (set->list (get-neighbors graph node)))])
      (send dc draw-line (coord-x (apply-positioning pos node))
            (coord-y (apply-positioning pos node))
            (coord-x (apply-positioning pos neighbors))
            (coord-y (apply-positioning pos neighbors))))))
            
    
(define L (build-list 25 identity))
(define positioning (random-positioning-of-node-list 800 700 L))
(define graph (grid 5 5))
(define my-relaxator (new-relaxator))

(define current-graph-type "grid")  ; to be implemented : add a node that depends on the type of the graph in the canvas

(define (generate-random)
  (define (this method . Largs)
    (case method
      ((tree) (list (complete-tree-graph (car Largs) (cadr Largs)) (random-positioning-of-node-list SIZE SIZE (build-list (calculate-nodes-tree (car Largs) (cadr Largs)) identity))))
      ((cyclic) (list (cyclic-graph (car Largs)) (random-positioning-of-node-list SIZE SIZE (build-list (car Largs) identity))))
      ((chain) (list (chain-graph (car Largs)) (random-positioning-of-node-list SIZE SIZE (build-list (car Largs) identity)) ))
      ((grid) (list (grid (car Largs) (cadr Largs)) (random-positioning-of-node-list SIZE SIZE (build-list (* (car Largs) (cadr Largs)) identity))))
      ((clic) (list (clic-graph (car Largs)) (random-positioning-of-node-list SIZE SIZE (build-list (car Largs) identity))))
      (else (display " "))))
  this)

(define my-graph (generate-random))
(define tmp (my-graph 'grid 10 10))
(set! graph (car tmp))
(define saved-graph (hash-copy graph))
(set! positioning (cadr tmp))
(define saved-positioning (hash-copy positioning))
(send FRAME show #t)
(define value 1)





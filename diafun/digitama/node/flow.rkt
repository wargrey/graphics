#lang typed/racket/base

(provide (all-defined-out))

(require "../node/style.rkt")
(require "../node/dc.rkt")

(require geofun/constructor)
(require geofun/composite)

(require geofun/digitama/convert)
(require geofun/digitama/geometry/anchor)
(require geofun/digitama/geometry/constants)
(require geofun/digitama/geometry/polygon/quadrilateral)
(require geofun/digitama/geometry/polygon/pentagon)
(require geofun/digitama/geometry/polygon/hexagon)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diaflow-block-in-arrow-label : (-> Symbol (Option Geo) Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum Dia:Node)
  (lambda [node-key label style width height]
    (create-dia-node dia:node:label
                     #:id node-key
                     (geo-rectangle #:id (dia-node-shape-id node-key)
                                    #:stroke (dia-node-select-stroke-paint style)
                                    #:fill (dia-node-select-fill-paint style)
                                    width height)
                     label)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diaflow-block-process : (-> Symbol (Option Geo) Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum Dia:Node)
  (lambda [node-key label style width height]
    (create-dia-node #:id node-key
                     (geo-rectangle #:id (dia-node-shape-id node-key)
                                    #:stroke (dia-node-select-stroke-paint style)
                                    #:fill (dia-node-select-fill-paint style)
                                    width height)
                     label)))

(define diaflow-block-subroutine : (-> Symbol (Option Geo) Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum Dia:Node)
  (lambda [node-key label style width height]
    (define stroke (dia-node-select-stroke-paint style))
    (define width-ratio : Nonnegative-Flonum 0.85)
    
    (create-dia-node #:id node-key
                     #:fit-ratio width-ratio 1.0
                     (geo-cc-superimpose #:id (dia-node-shape-id node-key)
                                         (geo-rectangle #:stroke stroke #:fill (dia-node-select-fill-paint style)
                                                        width height)
                                         (geo-rectangle #:stroke stroke #:fill #false
                                                        (* width width-ratio) height))
                     label)))

(define diaflow-block-decision : (-> Symbol (Option Geo) Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum Dia:Node)
  (lambda [node-key label style width height]
    (define vertices : Quadrilateral-Vertices (geo-rhombus-vertices width height))
    (define rhombus : Geo
      (geo-polygon #:id (dia-node-shape-id node-key)
                   #:stroke (dia-node-select-stroke-paint style)
                   #:fill (dia-node-select-fill-paint style)
                   #:window +nan.0+nan.0i
                   vertices))
    
    (create-dia-node dia:node:polygon
                     #:id node-key
                     #:intersect dia-polygon-intersect
                     #:fit-ratio 0.50 0.50
                     rhombus label vertices)))

(define diaflow-block-preparation : (-> Symbol (Option Geo) Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum Dia:Node)
  (lambda [node-key label style width height]
    (define vertices : Hexagon-Vertices (geo-hexagon-tile-vertices width height))
    (define hexagon : Geo
      (geo-polygon #:id (dia-node-shape-id node-key)
                   #:stroke (dia-node-select-stroke-paint style)
                   #:fill (dia-node-select-fill-paint style)
                   #:window +nan.0+nan.0i
                   vertices))
    
    (create-dia-node dia:node:polygon
                     #:id node-key
                     #:intersect dia-polygon-intersect
                     #:fit-ratio 0.75 1.00
                     hexagon label vertices)))

(define diaflow-block-terminal : (-> Symbol (Option Geo) Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum Dia:Node)
  (lambda [node-key label style width height]
    (define r : Flonum (* height 0.5))
    (create-dia-node #:id node-key
                     #:fit-ratio (max (/ (- width r) width) 0.0) 1.00
                     (geo-stadium #:id (dia-node-shape-id node-key)
                                  #:stroke (dia-node-select-stroke-paint style)
                                  #:fill (dia-node-select-fill-paint style)
                                  (- width (* r 2.0)) r)
                     label)))

(define diaflow-block-dataIO : (-> Symbol (Option Geo) Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum Dia:Node)
  (lambda [node-key label style width height]
    (define vertices : Quadrilateral-Vertices (geo-parallelogram-vertices width height (/ pi 3.0)))
    (define parallelogram : Geo
      (geo-polygon #:id (dia-node-shape-id node-key)
                   #:stroke (dia-node-select-stroke-paint style)
                   #:fill (dia-node-select-fill-paint style)
                   #:window +nan.0+nan.0i
                   vertices))
    
    (create-dia-node dia:node:polygon
                     #:id node-key
                     #:intersect dia-polygon-intersect
                     #:fit-ratio (max (- 1.0 (/ (* height (sqrt 3.0) 2/3) width)) 0.0) 1.0
                     parallelogram label vertices)))

(define diaflow-block-inspection : (-> Symbol (Option Geo) Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum (Option Symbol) Dia:Node)
  (lambda [node-key label style width height hint]
    (define r : Nonnegative-Flonum (* (min width height) 0.5))
    (create-dia-node dia:node:circle
                     #:id node-key
                     #:intersect dia-circle-intersect
                     #:fit-ratio 0.75 0.75
                     (geo-circle #:id (dia-node-shape-id node-key)
                                  #:stroke (dia-node-select-stroke-paint style)
                                  #:fill (dia-node-select-fill-paint style)
                                  r)
                     label r)))

(define diaflow-block-reference : (-> Symbol (Option Geo) Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum (Option Symbol) Dia:Node)
  (lambda [node-key label style width height hint]
    (define ratio : Nonnegative-Flonum 0.618)
    (define vertices : Pentagon-Vertices (geo-invhouse-vertices width height ratio))
    (define house : Geo
      (geo-polygon #:id (dia-node-shape-id node-key)
                   #:stroke (dia-node-select-stroke-paint style)
                   #:fill (dia-node-select-fill-paint style)
                   #:window +nan.0+nan.0i
                   vertices))
    
    (create-dia-node dia:node:polygon
                     #:id node-key
                     #:intersect dia-polygon-intersect
                     #:fit-ratio 1.00 ratio
                     #:position 0.5 (* ratio 0.5)
                     house label vertices)))

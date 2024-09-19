#lang typed/racket/base

(provide (all-defined-out))

(require "../node/style.rkt")
(require "../node/dc.rkt")

(require geofun/constructor)

(require geofun/digitama/convert)
(require geofun/digitama/geometry/anchor)
(require geofun/digitama/geometry/constants)
(require geofun/digitama/geometry/polygon/quadrilateral)
(require geofun/digitama/geometry/polygon/hexagon)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diaflow-block-process : (-> Symbol (Option Geo) Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum Dia:Node)
  (lambda [node-key label style width height]
    (create-dia-node #:id node-key
                     (geo-rectangle #:id (diaflow-node-shape-id node-key)
                                    #:stroke (dia-node-select-stroke-paint style)
                                    #:fill (dia-node-select-fill-paint style)
                                    width height)
                     label)))

(define diaflow-block-decision : (-> Symbol (Option Geo) Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum Dia:Node)
  (lambda [node-key label style width height]
    (define vertices : (Listof Float-Complex) (geo-rhombus-vertices width height))
    (define rhombus : Geo
      (geo-polygon #:id (diaflow-node-shape-id node-key)
                   #:stroke (dia-node-select-stroke-paint style)
                   #:fill (dia-node-select-fill-paint style)
                   #:window +nan.0+nan.0i
                   vertices))
    
    (create-dia-node dia:node:polygon
                     #:id node-key
                     #:fit-ratio 0.50 0.50
                     rhombus label vertices)))

(define diaflow-block-preparation : (-> Symbol (Option Geo) Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum Dia:Node)
  (lambda [node-key label style width height]
    (define vertices : (Listof Float-Complex) (geo-hexagon-tile-vertices width height))
    (define hexagon : Geo
      (geo-polygon #:id (diaflow-node-shape-id node-key)
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
                     (geo-stadium #:id (diaflow-node-shape-id node-key)
                                  #:stroke (dia-node-select-stroke-paint style)
                                  #:fill (dia-node-select-fill-paint style)
                                  (- width (* r 2.0)) r)
                     label)))

(define diaflow-block-dataIO : (-> Symbol (Option Geo) Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum Dia:Node)
  (lambda [node-key label style width height]
    (define vertices : (Listof Float-Complex) (geo-parallelogram-vertices width height (/ pi 3.0)))
    (define parallelogram : Geo
      (geo-polygon #:id (diaflow-node-shape-id node-key)
                   #:stroke (dia-node-select-stroke-paint style)
                   #:fill (dia-node-select-fill-paint style)
                   #:window +nan.0+nan.0i
                   vertices))
    
    (create-dia-node dia:node:polygon
                     #:id node-key
                     #:intersect dia-polygon-intersect
                     #:fit-ratio (max (- 1.0 (/ (* height (sqrt 3.0) 2/3) width)) 0.0) 1.0
                     parallelogram label vertices)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diaflow-node-shape-id : (-> Symbol Symbol)
  (lambda [node-key]
    (string->symbol (string-append "&" (geo-anchor->string node-key)))))

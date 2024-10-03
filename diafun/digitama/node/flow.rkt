#lang typed/racket/base

(provide (all-defined-out))

(require "../node/style.rkt")
(require "../node/dc.rkt")

(require geofun/constructor)
(require geofun/composite)

(require geofun/digitama/convert)
(require geofun/digitama/geometry/constants)
(require geofun/digitama/geometry/polygon/quadrilateral)
(require geofun/digitama/geometry/polygon/pentagon)
(require geofun/digitama/geometry/polygon/hexagon)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diaflow-block-process : (-> Symbol (Option Geo) Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum (Option Symbol) Dia:Node)
  (lambda [node-key label style width height hint]
    (create-dia-node #:id node-key #:type 'Process hint
                     (geo-rectangle #:id (dia-node-shape-id node-key)
                                    #:stroke (dia-node-select-stroke-paint style)
                                    #:fill (dia-node-select-fill-paint style)
                                    width height)
                     label)))

(define diaflow-block-prefab : (-> Symbol (Option Geo) Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum (Option Symbol) Dia:Node)
  (lambda [node-key label style width height hint]
    (define stroke (dia-node-select-stroke-paint style))
    (define width-ratio : Nonnegative-Flonum 0.85)
    
    (create-dia-node #:id node-key #:type 'Prefab hint
                     #:fit-ratio width-ratio 1.0
                     (geo-cc-superimpose #:id (dia-node-shape-id node-key)
                                         (geo-rectangle width height #:stroke stroke #:fill (dia-node-select-fill-paint style))
                                         (geo-rectangle (* width width-ratio) height #:stroke stroke #:fill #false))
                     label)))

(define diaflow-block-alternate : (-> Symbol (Option Geo) Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum (Option Symbol) Dia:Node)
  (lambda [node-key label style width height hint]
    (define stroke (dia-node-select-stroke-paint style))
    (define width-ratio : Nonnegative-Flonum 0.85)
    
    (create-dia-node #:id node-key #:type 'Alternate hint
                     #:fit-ratio width-ratio 1.0
                     (geo-cc-superimpose #:id (dia-node-shape-id node-key)
                                         (geo-rectangle width height -0.1618 #:stroke stroke #:fill (dia-node-select-fill-paint style)))
                     label)))

(define diaflow-block-decision : (-> Symbol (Option Geo) Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum (Option Symbol) Dia:Node)
  (lambda [node-key label style width height hint]
    (define vertices : Quadrilateral-Vertices (geo-rhombus-vertices width height))
    
    (create-dia-node dia:node:polygon
                     #:id node-key  #:type 'Decision hint
                     #:intersect dia-polygon-intersect
                     #:fit-ratio 0.64 0.75
                     (diaflow-polygon-shape node-key style vertices) label vertices)))

(define diaflow-block-preparation : (-> Symbol (Option Geo) Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum (Option Symbol) Dia:Node)
  (lambda [node-key label style width height hint]
    (define vertices : Hexagon-Vertices (geo-hexagon-tile-vertices width height))
    
    (create-dia-node dia:node:polygon
                     #:id node-key #:type 'Preparation hint
                     #:intersect dia-polygon-intersect
                     #:fit-ratio 0.75 1.00
                     (diaflow-polygon-shape node-key style vertices) label vertices)))

(define diaflow-block-terminal : (-> Symbol (Option Geo) Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum (Option Symbol) Dia:Node)
  (lambda [node-key label style width height hint]
    (define r : Flonum (* height 0.5))
    (create-dia-node #:id node-key #:type 'Terminal hint
                     #:fit-ratio (max (/ (- width r) width) 0.0) 1.00
                     (geo-stadium #:id (dia-node-shape-id node-key)
                                  #:stroke (dia-node-select-stroke-paint style)
                                  #:fill (dia-node-select-fill-paint style)
                                  (- width (* r 2.0)) r)
                     label)))

(define diaflow-block-input : (-> Symbol (Option Geo) Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum (Option Symbol) Dia:Node)
  (lambda [node-key label style width height hint]
    (if (eq? hint 'manual)
        (let* ([ratio 0.75]
               [vertices (geo-keyboard-vertices width height ratio)])
          (create-dia-node dia:node:polygon
                           #:id node-key #:type 'Input hint
                           #:intersect dia-polygon-intersect
                           #:fit-ratio 1.0 ratio
                           #:position 0.5 (max (- 1.0 (* ratio 0.5)) 0.0)
                           (diaflow-polygon-shape node-key style vertices)
                           label vertices))
        (diaflow-block-dataIO node-key label style width height 'Input hint))))

(define diaflow-block-output : (-> Symbol (Option Geo) Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum (Option Symbol) Dia:Node)
  (lambda [node-key label style width height hint]
    (diaflow-block-dataIO node-key label style width height 'Output hint)))

(define diaflow-block-inspection : (-> Symbol (Option Geo) Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum (Option Symbol) Dia:Node)
  (lambda [node-key label style width height hint]
    (define r : Nonnegative-Flonum (* (min width height) 0.5))
    (create-dia-node dia:node:circle
                     #:id node-key #:type 'Inspection hint
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
    (define vertices : Pentagon-Vertices (geo-house-vertices width height (- ratio)))
    
    (create-dia-node dia:node:polygon
                     #:id node-key #:type 'Reference hint
                     #:intersect dia-polygon-intersect
                     #:fit-ratio 1.00 ratio
                     #:position 0.5 (* ratio 0.5)
                     (diaflow-polygon-shape node-key style vertices) label vertices)))

(define diaflow-block-manual-operation : (-> Symbol (Option Geo) Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum (Option Symbol) Dia:Node)
  (lambda [node-key label style width height hint]
    (define ratio : Nonnegative-Flonum 0.75)
    (define vertices : Quadrilateral-Vertices (geo-isosceles-trapezium-vertices width height (max (/ 1.0 ratio) 0.0)))
    
    (create-dia-node dia:node:polygon
                     #:id node-key #:type 'Operation hint
                     #:intersect dia-polygon-intersect
                     #:fit-ratio ratio 1.00
                     (diaflow-polygon-shape node-key style vertices) label vertices)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diaflow-block-dataIO : (-> Symbol (Option Geo) Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum Symbol (Option Symbol) Dia:Node)
  (lambda [node-key label style width height type subtype]
    (define vertices : Quadrilateral-Vertices (geo-parallelogram-vertices width height (/ pi 3.0)))
    
    (create-dia-node dia:node:polygon
                     #:id node-key #:type type subtype
                     #:intersect dia-polygon-intersect
                     #:fit-ratio (max (- 1.0 (/ (* height (sqrt 3.0) 2/3) width)) 0.0) 1.0
                     (diaflow-polygon-shape node-key style vertices) label vertices)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diaflow-polygon-shape : (-> Symbol Dia-Node-Style (Listof Float-Complex) Geo)
  (lambda [node-key style vertices]
    (geo-polygon #:id (dia-node-shape-id node-key)
                 #:stroke (dia-node-select-stroke-paint style)
                 #:fill (dia-node-select-fill-paint style)
                 #:window +nan.0+nan.0i
                 vertices)))

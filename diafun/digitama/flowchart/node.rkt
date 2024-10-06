#lang typed/racket/base

(provide (all-defined-out))

(require "../node/style.rkt")
(require "../node/dc.rkt")
(require "interface.rkt")

(require geofun/constructor)
(require geofun/paint)
(require geofun/stroke)

(require geofun/digitama/convert)
(require geofun/digitama/geometry/constants)
(require geofun/digitama/geometry/polygon/triangle)
(require geofun/digitama/geometry/polygon/quadrilateral)
(require geofun/digitama/geometry/polygon/pentagon)
(require geofun/digitama/geometry/polygon/hexagon)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diaflow-block-process : DiaFlow-Block-Create
  (lambda [node-key label style width height direction hint]
    (cond [(eq? hint 'Predefined) (diaflow-block-prefab node-key label style width height direction hint)]
          [(eq? hint 'Alternate)  (diaflow-block-alternate node-key label style width height direction hint)]
          [else (create-dia-node #:id node-key #:type 'Process hint
                                 (geo-rectangle #:id (dia-node-shape-id node-key)
                                                #:stroke (dia-node-select-stroke-paint style)
                                                #:fill (dia-node-select-fill-paint style)
                                                width height)
                                 label)])))

(define diaflow-block-prefab : DiaFlow-Block-Create
  (lambda [node-key label style width height direction hint]
    (define width-ratio : Nonnegative-Flonum 0.84)
    (define vline : Flonum (* (- 1.0 width-ratio) 0.5))
    
    (create-dia-node #:id node-key #:type 'Process hint
                     #:fit-ratio width-ratio 1.0
                     (geo-rectangle #:id (dia-node-shape-id node-key) #:vlines (list vline (- vline))
                                    #:stroke (dia-node-select-stroke-paint style)
                                    #:fill (dia-node-select-fill-paint style)
                                    width height)
                     label)))

(define diaflow-block-alternate : DiaFlow-Block-Create
  (lambda [node-key label style width height direction hint]
    (create-dia-node #:id node-key #:type 'Alternate hint
                     #:fit-ratio 0.85 1.0
                     (geo-rectangle #:id (dia-node-shape-id node-key)
                                    #:stroke (dia-node-select-stroke-paint style)
                                    #:fill (dia-node-select-fill-paint style)
                                    width height -0.25)
                     label)))

(define diaflow-block-decision : DiaFlow-Block-Create
  (lambda [node-key label style width height direction hint]
    (define-values (w h stroke) (diaflow-polygon-size width height style))
    (define vertices : Quadrilateral-Vertices (geo-rhombus-vertices w h))
    
    (create-dia-node dia:node:polygon
                     #:id node-key  #:type 'Decision hint
                     #:intersect dia-polygon-intersect
                     #:fit-ratio 0.64 0.75
                     (diaflow-polygon-shape node-key style stroke vertices) label vertices)))

(define diaflow-block-preparation : DiaFlow-Block-Create
  (lambda [node-key label style width height direction hint]
    (define-values (w h stroke) (diaflow-polygon-size width height style))
    (define vertices : Hexagon-Vertices (geo-hexagon-tile-vertices w h))

    ; TODO: these vertices have trouble in intersecting with arrows
    (create-dia-node ;dia:node:polygon
                     #:id node-key #:type 'Preparation hint
                     ;#:intersect dia-polygon-intersect
                     #:fit-ratio 0.75 1.00
                     (diaflow-polygon-shape node-key style stroke vertices) label #;vertices)))

(define diaflow-block-terminal : DiaFlow-Block-Create
  (lambda [node-key label style width height direction hint]
    (define r : Flonum (* height 0.5))
    (create-dia-node #:id node-key #:type 'Terminal hint
                     #:fit-ratio (max (/ (- width r) width) 0.0) 1.00
                     (geo-stadium #:id (dia-node-shape-id node-key)
                                  #:stroke (dia-node-select-stroke-paint style)
                                  #:fill (dia-node-select-fill-paint style)
                                  (- width (* r 2.0)) r)
                     label)))

(define diaflow-block-input : DiaFlow-Block-Create
  (lambda [node-key label style width height direction hint]
    (if (eq? hint 'user)
        (let*-values ([(ratio) 0.75]
                      [(w h stroke) (diaflow-polygon-size width height style)]
                      [(vertices) (geo-keyboard-vertices w h ratio)])
          (create-dia-node dia:node:polygon
                           #:id node-key #:type 'Input hint
                           #:intersect dia-polygon-intersect
                           #:fit-ratio 1.0 ratio
                           #:position 0.5 (max (- 1.0 (* ratio 0.5)) 0.0)
                           (diaflow-polygon-shape node-key style stroke vertices)
                           label vertices))
        (diaflow-block-dataIO node-key label style width height 'Input hint))))

(define diaflow-block-output : DiaFlow-Block-Create
  (lambda [node-key label style width height direction hint]
    (if (eq? hint 'user)
        (let-values ([(ogive r) (values (* width 0.384) (* height 0.5))])
          (create-dia-node #:id node-key #:type 'Output hint
                           #:fit-ratio 0.75 1.00
                           (geo-bullet #:id (dia-node-shape-id node-key)
                                       #:stroke (dia-node-select-stroke-paint style)
                                       #:fill (dia-node-select-fill-paint style)
                                       ogive r (- width ogive))
                           label))
        (diaflow-block-dataIO node-key label style width height 'Output hint))))

(define diaflow-block-inspection : DiaFlow-Block-Create
  (lambda [node-key label style width height direction hint]
    (define r : Nonnegative-Flonum (* (min width height) 0.5))
    (create-dia-node dia:node:circle
                     #:id node-key #:type 'Connector (or hint 'Inspection)
                     #:intersect dia-circle-intersect
                     #:fit-ratio 0.75 0.75
                     (geo-circle #:id (dia-node-shape-id node-key)
                                  #:stroke (dia-node-select-stroke-paint style)
                                  #:fill (dia-node-select-fill-paint style)
                                  r)
                     label r)))

(define diaflow-block-reference : DiaFlow-Block-Create
  (lambda [node-key label style width height direction hint]
    (define ratio : Nonnegative-Flonum 0.618)
    (define-values (w h stroke) (diaflow-polygon-size width height style))
    (define vertices : Pentagon-Vertices (geo-house-vertices w h (- ratio)))
    
    (create-dia-node dia:node:polygon
                     #:id node-key #:type 'Connector (or hint 'Reference)
                     #:intersect dia-polygon-intersect
                     #:fit-ratio 1.00 ratio
                     #:position 0.5 (* ratio 0.5)
                     (diaflow-polygon-shape node-key style stroke vertices) label vertices)))

(define diaflow-block-selection : DiaFlow-Block-Create
  (lambda [node-key label style width height direction hint]
    (define r : Nonnegative-Flonum (* (min width height) 0.5))
    (create-dia-node dia:node:circle
                     #:id node-key #:type 'Selection hint
                     #:intersect dia-circle-intersect
                     #:fit-ratio 0.75 0.75
                     (geo-circle #:id (dia-node-shape-id node-key)
                                  #:stroke (dia-node-select-stroke-paint style)
                                  #:fill (dia-node-select-fill-paint style)
                                  #:diameters (list 0.0 pi/2)
                                  r)
                     #false r)))

(define diaflow-block-junction : DiaFlow-Block-Create
  (lambda [node-key label style width height direction hint]
    (define r : Nonnegative-Flonum (* (min width height) 0.5))
    (create-dia-node dia:node:circle
                     #:id node-key #:type 'Junction hint
                     #:intersect dia-circle-intersect
                     #:fit-ratio 0.75 0.75
                     (geo-circle #:id (dia-node-shape-id node-key)
                                  #:stroke (dia-node-select-stroke-paint style)
                                  #:fill (dia-node-select-fill-paint style)
                                  #:diameters (list pi/4 3pi/4)
                                  r)
                     #false r)))

(define diaflow-block-manual-operation : DiaFlow-Block-Create
  (lambda [node-key label style width height direction hint]
    (define ratio : Nonnegative-Flonum 0.75)
    (define-values (w h stroke) (diaflow-polygon-size width height style))
    (define vertices : Quadrilateral-Vertices (geo-isosceles-trapezium-vertices w h (max (/ 1.0 ratio) 0.0)))
    
    (create-dia-node dia:node:polygon
                     #:id node-key #:type 'Operation hint
                     #:intersect dia-polygon-intersect
                     #:fit-ratio ratio 1.00
                     (diaflow-polygon-shape node-key style stroke vertices) label vertices)))

(define diaflow-block-extract : DiaFlow-Block-Create
  (lambda [node-key label style width height direction hint]
    (define-values (w h stroke) (diaflow-polygon-size width height style))
    (define vertices : Triangle-Vertices (geo-isosceles-upwards-triangle-vertices w h))
    
    (create-dia-node dia:node:polygon
                     #:id node-key #:type 'Extract hint
                     #:intersect dia-polygon-intersect
                     (diaflow-polygon-shape node-key style stroke vertices) #false vertices)))

(define diaflow-block-merge : DiaFlow-Block-Create
  (lambda [node-key label style width height direction hint]
    (define-values (w h stroke) (diaflow-polygon-size width height style))
    (define vertices : Triangle-Vertices (geo-isosceles-downwards-triangle-vertices w h))
    
    (create-dia-node dia:node:polygon
                     #:id node-key #:type 'Merge hint
                     #:intersect dia-polygon-intersect
                     (diaflow-polygon-shape node-key style stroke vertices) #false vertices)))

(define diaflow-block-delay : DiaFlow-Block-Create
  (lambda [node-key label style width height direction hint]
    (define r : Flonum (* height 0.5))
    (create-dia-node #:id node-key #:type 'Delay hint
                     #:fit-ratio (max (/ (- width r) width) 0.0) 1.00
                     (geo-half-stadium #:id (dia-node-shape-id node-key)
                                       #:stroke (dia-node-select-stroke-paint style)
                                       #:fill (dia-node-select-fill-paint style)
                                       (- width r) r)
                     label)))

(define diaflow-block-storage : DiaFlow-Block-Create
  (lambda [node-key label style width height direction hint]
    (cond [(eq? hint 'Memory) (diaflow-block-memory node-key label style width height direction hint)]
          [(eq? hint 'File) (diaflow-block-document node-key label style width height direction hint)]
          [(eq? hint 'Directory) (diaflow-block-multiple-document node-key label style width height direction hint)]
          [(eq? hint 'Database) (diaflow-block-database node-key label style width height direction hint)]
          [else (let ([aradius (* height 0.5 0.384)])
                  (create-dia-node #:id node-key #:type 'Stroage hint
                                   #:position (max (- 0.5 (* (/ aradius width) 0.5)) 0.0) 0.5
                                   (geo-storage #:id (dia-node-shape-id node-key)
                                                #:stroke (dia-node-select-stroke-paint style)
                                                #:fill (dia-node-select-fill-paint style)
                                                width height aradius)
                                   label))])))

(define diaflow-block-memory : DiaFlow-Block-Create
  (lambda [node-key label style width height direction hint]
    (define line-ratio : Nonnegative-Flonum 0.1618)
    (define label-pos : Nonnegative-Flonum (min (* width line-ratio) (* height line-ratio)))
    (define label-zone-width : Flonum  (- width label-pos))
    (define label-zone-height : Flonum (- height label-pos))
    
    (create-dia-node #:id node-key #:type 'Storage hint
                     #:fit-ratio (max (/ label-zone-width width) 0.0) (max (/ label-zone-width height) 0.0)
                     #:position (max (/ (+ label-pos (* label-zone-width 0.5)) width) 0.0) (max (/ (+ label-pos (* label-zone-height 0.5)) height) 0.0)
                     (geo-rectangle #:id (dia-node-shape-id node-key) #:vlines (list label-pos) #:hlines (list label-pos)
                                    #:stroke (dia-node-select-stroke-paint style)
                                    #:fill (dia-node-select-fill-paint style)
                                    width height)
                     label)))

(define diaflow-block-document : DiaFlow-Block-Create
  (lambda [node-key label style width height direction hint]
    (define hratio : Nonnegative-Flonum 0.75)
    (create-dia-node #:id node-key #:type 'Storage hint
                     #:fit-ratio 1.0 hratio
                     #:position 0.5 (* hratio 0.5)
                     (geo-document #:id (dia-node-shape-id node-key)
                                   #:stroke (dia-node-select-stroke-paint style)
                                   #:fill (dia-node-select-fill-paint style)
                                   width height (* (- hratio 1.0) 0.5))
                     label)))

(define diaflow-block-multiple-document : DiaFlow-Block-Create
  (lambda [node-key label style width height direction hint]
    (define extra-n : Index 2)
    (define gapsize : Nonnegative-Flonum (* height 0.1))
    (define ngap : Nonnegative-Flonum (* gapsize (real->double-flonum extra-n)))
    (define wgr : Flonum (/ ngap width))
    (define hgr : Flonum (/ ngap width))
    (define wave-ratio : Nonnegative-Flonum 0.25)
    (define wratio : Nonnegative-Flonum (abs (- 1.00 wgr)))
    (define hratio : Nonnegative-Flonum (abs (- 1.00 wave-ratio hgr)))
    
    (create-dia-node #:id node-key #:type 'Storage hint
                     #:fit-ratio wratio hratio
                     #:position (abs (- 0.5 (* wgr 0.5))) (abs (+ hgr (* hratio 0.5)))
                     (geo-document #:id (dia-node-shape-id node-key)
                                   #:stroke (dia-node-select-stroke-paint style)
                                   #:fill (dia-node-select-fill-paint style)
                                   #:gapsize gapsize
                                   #:extra-n extra-n
                                   width height (* wave-ratio -0.5))
                     label)))

(define diaflow-block-database : DiaFlow-Block-Create
  (lambda [node-key label style width height direction hint]
    (define extra-n : Index 2)
    (define bradius : Nonnegative-Flonum (* height 0.1618))
    (define gapsize : Nonnegative-Flonum (* bradius 0.618))
    (define label-zone-height : Flonum (- height (* bradius 3.0) (* gapsize (exact->inexact extra-n))))
    (define hratio : Nonnegative-Flonum (abs (/ label-zone-height height)))
    
    (create-dia-node #:id node-key #:type 'Storage hint
                     #:fit-ratio 1.0 hratio
                     #:position 0.5 (+ 0.5 (* hratio 0.5))
                     (geo-database #:id (dia-node-shape-id node-key)
                                   #:stroke (dia-node-select-stroke-paint style)
                                   #:fill (dia-node-select-fill-paint style)
                                   #:extra-n extra-n #:gapsize gapsize
                                   width height bradius)
                     label)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diaflow-block-dataIO : (-> Symbol (Option Geo) Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum Symbol (Option Symbol) Dia:Node)
  (lambda [node-key label style width height type subtype]
    (define-values (w h stroke) (diaflow-polygon-size width height style))
    (define vertices : Quadrilateral-Vertices (geo-parallelogram-vertices w h (/ pi 3.0)))
    
    (create-dia-node dia:node:polygon
                     #:id node-key #:type type subtype
                     #:intersect dia-polygon-intersect
                     #:fit-ratio (max (- 1.0 (/ (* height (sqrt 3.0) 2/3) width)) 0.0) 1.0
                     (diaflow-polygon-shape node-key style stroke vertices) label vertices)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diaflow-polygon-size : (-> Nonnegative-Flonum Nonnegative-Flonum Dia-Node-Style (Values Nonnegative-Flonum Nonnegative-Flonum Maybe-Stroke-Paint))
  (lambda [width height style]
    (define stroke (dia-node-select-stroke-paint style))

    (if (stroke? stroke)
        (let ([thickness (stroke-width stroke)])
          (values (max (- width thickness) 0.0) (max (- height thickness) 0.0) stroke))
        (values width height stroke))))

(define diaflow-polygon-shape : (-> Symbol Dia-Node-Style Maybe-Stroke-Paint (Listof Float-Complex) Geo)
  (lambda [node-key style stroke vertices]
    (geo-polygon #:id (dia-node-shape-id node-key)
                 #:stroke stroke
                 #:fill (dia-node-select-fill-paint style)
                 #:window +nan.0+nan.0i
                 vertices)))

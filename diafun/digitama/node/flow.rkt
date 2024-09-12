#lang typed/racket/base

(provide (all-defined-out))

(require "../node/style.rkt")

(require geofun/digitama/geometry/anchor)
(require geofun/digitama/geometry/constants)
(require geofun/digitama/geometry/polygon/quadrilateral)

(require geofun/digitama/layer/combine)
(require geofun/digitama/dc/composite)
(require geofun/digitama/convert)
(require geofun/digitama/resize)

(require geofun/composite)
(require geofun/constructor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diaflow-block-process : (-> Symbol Geo:Text Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum Geo)
  (lambda [node-key label style width height]
    (geo-cc-superimpose #:id node-key
                        (geo-rectangle #:id (diaflow-node-id node-key)
                                       #:stroke (dia-node-select-stroke-paint style)
                                       #:fill (dia-node-select-fill-paint style)
                                       width height)
                        label)))

(define diaflow-block-decision : (-> Symbol Geo:Text Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum Geo)
  (lambda [node-key label style width height]
    (geo-cc-superimpose #:id node-key
                        (geo-rhombus #:id (diaflow-node-id node-key)
                                     #:stroke (dia-node-select-stroke-paint style)
                                     #:fill (dia-node-select-fill-paint style)
                                     width height)
                        label)))

(define diaflow-block-preparation : (-> Symbol Geo:Text Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum Geo)
  (lambda [node-key label style width height]
    (geo-cc-superimpose #:id node-key
                        (geo-hexagon-tile #:id (diaflow-node-id node-key)
                                          #:stroke (dia-node-select-stroke-paint style)
                                          #:fill (dia-node-select-fill-paint style)
                                          width height)
                        label)))

(define diaflow-block-terminal : (-> Symbol Geo:Text Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum Geo)
  (lambda [node-key label style width height]
    (define r : Flonum (* height 0.5))
    (geo-cc-superimpose #:id node-key
                        (geo-stadium #:id (diaflow-node-id node-key)
                                     #:stroke (dia-node-select-stroke-paint style)
                                     #:fill (dia-node-select-fill-paint style)
                                     (- width (* r 2.0)) r)
                        label)))

(define diaflow-block-dataIO : (-> Symbol Geo:Text Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum Geo)
  (lambda [node-key label style width height]
    (define vertices (geo-parallelogram-vertices width height (/ pi 3.0)))
    (define shape : Geo
      (geo-polygon #:id (diaflow-node-id node-key)
                   #:stroke (dia-node-select-stroke-paint style) #:fill (dia-node-select-fill-paint style) #:window +nan.0+nan.0i
                   vertices))
    
    (make-geo:group node-key #false (geo-composite-layers shape label 0.5 0.5 0.5 0.5))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diaflow-node-id : (-> Symbol Symbol)
  (lambda [node-key]
    (string->symbol (string-append "&" (geo-anchor->string node-key)))))

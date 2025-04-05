#lang typed/racket/base

(provide (all-defined-out))

(require "../node/style.rkt")
(require "../node/dc.rkt")
(require "../path/interface.rkt")

(require geofun/digitama/base)
(require geofun/digitama/dc/arc)
(require geofun/digitama/dc/stickman)

(require geofun/paint)
(require geofun/stroke)
(require geofun/color)
(require geofun/composite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diauc-block-actor : Dia-Path-Block-Create
  (lambda [node-key label style width height direction hint]
    (define head-color : Maybe-Fill-Paint (dia-node-select-fill-paint style))
    (define body-color (if (color? head-color) (rgb-transform-scale head-color 0.75) head-color))
    
    (define stickman : Geo:Stickman
      (geo-stickman #:id (dia-node-shape-id node-key)
                    #:fallback-border-thickness (dia-node-select-stroke-width style)
                    #:stroke (dia-node-select-stroke-paint style)
                    #:head-color head-color
                    #:body-color body-color
                    #:arm-color head-color
                    height))
    
    (create-dia-node #:id node-key #:type 'Actor hint
                     (if (not label) stickman (geo-vc-append stickman label))
                     #false)))

(define diauc-block-ucase : Dia-Path-Block-Create
  (lambda [node-key label style width height direction hint]
    (define pen (dia-node-select-stroke-paint style))
    (define thickness (stroke-maybe-width pen))
    
    (create-dia-node #:node dia:node:ellipse
                     #:id node-key #:type 'UseCase hint
                     #:intersect dia-ellipse-intersect
                     #:fit-ratio 0.81 0.81
                     (geo-ellipse #:id (dia-node-shape-id node-key)
                                  #:stroke pen
                                  #:fill (dia-node-select-fill-paint style)
                                  width height)
                     label (+ (* width 0.5) thickness) (+ (* height 0.5) thickness))))

#lang typed/racket/base

(provide (all-defined-out))

(require "../block/style.rkt")
(require "../block/dc.rkt")
(require "../interface.rkt")

(require geofun/digitama/dc/arc)
(require geofun/digitama/dc/stickman)
(require geofun/digitama/paint/self)

(require geofun/paint)
(require geofun/color)
(require geofun/composite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diauc-block-actor : Dia-Block-Create
  (lambda [block-key brief style width height direction subtype]
    (define head-color : Maybe-Fill-Paint (dia-block-select-fill-paint style))
    (define body-color (if (color? head-color) (rgb-transform-scale head-color 0.75) head-color))
    
    (define stickman : Geo:Stickman
      (geo-stickman #:id (dia-block-shape-id block-key)
                    #:fallback-border-thickness (dia-block-select-stroke-width style)
                    #:stroke (dia-block-select-stroke-paint style)
                    #:head-color head-color
                    #:body-color body-color
                    #:arm-color head-color
                    height))
    
    (create-dia-block #:id block-key #:type 'Actor subtype
                      (if (not brief) stickman (geo-vc-append stickman brief))
                      #false)))

(define diauc-block-ucase : Dia-Block-Create
  (lambda [block-key brief style width height direction subtype]
    (define pen (dia-block-select-stroke-paint style))
    (define thickness (pen-maybe-width pen))
    
    (create-dia-block #:block dia:block:ellipse
                      #:id block-key #:type 'UseCase subtype
                      #:intersect dia-ellipse-intersect
                      #:fit-ratio 0.81 0.81
                      (geo-ellipse #:id (dia-block-shape-id block-key)
                                   #:stroke pen
                                   #:fill (dia-block-select-fill-paint style)
                                   width height)
                      brief (+ (* width 0.5) thickness) (+ (* height 0.5) thickness))))

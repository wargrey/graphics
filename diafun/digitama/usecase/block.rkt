#lang typed/racket/base

(provide (all-defined-out))

(require "../block/style.rkt")
(require "../block/dc.rkt")
(require "../interface.rkt")

(require geofun/digitama/dc/arc)
(require geofun/digitama/dc/stickman)
(require geofun/digitama/paint/self)

(require geofun/color)
(require geofun/composite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diauc-block-actor : Dia-Block-Create
  (lambda [block-key caption style width height direction subtype]
    (define head-color (dia-block-resolve-fill-paint style))
    (define body-color (if (color? head-color) (rgb-transform-scale head-color 0.75) head-color))
    
    (define stickman : Geo:Stickman
      (geo-stickman #:id (dia-block-shape-id block-key)
                    #:fallback-border-thickness (dia-block-resolve-stroke-width style)
                    #:stroke (dia-block-resolve-stroke-paint style)
                    #:head-color head-color
                    #:body-color body-color
                    #:arm-color head-color
                    height))
    
    (create-dia-block #:id block-key #:type 'Actor subtype
                      #:with (cond [(not caption) stickman]
                                   [else (geo-vc-append #:desc (dia-block-desc-from-caption caption)
                                                        stickman caption)])
                      #false)))

(define diauc-block-ucase : Dia-Block-Create
  (lambda [block-key caption style width height direction subtype]
    (define pen (dia-block-resolve-stroke-paint style))
    (define thickness (pen-maybe-width pen))
    
    (create-dia-block #:block dia:block:ellipse
                      #:id block-key #:type 'UseCase subtype
                      #:intersect dia-ellipse-intersect
                      #:fit-region 0.81 0.81
                      #:with (geo-ellipse #:id (dia-block-shape-id block-key)
                                          #:stroke pen
                                          #:fill (dia-block-resolve-fill-paint style)
                                          width height)
                      caption (+ (* width 0.5) thickness) (+ (* height 0.5) thickness))))

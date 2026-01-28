#lang typed/racket/base

(provide (all-defined-out))

(require "../dc.rkt")
(require "../style.rkt")

(require geofun/color)
(require geofun/composite)
(require geofun/constructor)
(require geofun/resize)

(require geofun/digitama/base)
(require geofun/digitama/self)

(require geofun/digitama/geometry/sides)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-actor-stickman : (->* (Symbol (Option Geo) (Dia-Block-Style-Spec S) Nonnegative-Flonum Nonnegative-Flonum)
                                               (Any)
                                               Dia:Block)
  (lambda [id caption style width height [tags #false]]
    (define head-color (dia-block-resolve-fill-paint style))
    (define body-color (if (color? head-color) (rgb-transform-scale head-color 0.75) head-color))
    (define padding (dia-block-resolve-padding style))
    
    (define stickman : Geo:Stickman
      (geo-stickman #:id (dia-block-shape-id id)
                    #:fallback-border-thickness (dia-block-resolve-stroke-width style)
                    #:stroke (dia-block-resolve-stroke-paint style)
                    #:head-color head-color
                    #:body-color body-color
                    #:arm-color head-color
                    height))

    (define stickman-width (geo-width stickman))
    (define caption-width (if (or caption) (geo-width caption) 0.0))

    (cond [(not caption)
           (create-dia-block #:id id tags #:with style stickman #false)]
          [(<= caption-width width)
           (create-dia-block #:id id tags
                             #:intersect (if (< stickman-width caption-width)
                                             (dia-apothem-intersect (abs (/ stickman-width caption-width 2.0)) 0.5)
                                             dia-default-intersect)
                             #:with-group style (geo-vc-append #:desc (dia-block-desc-from-caption caption)
                                                               #:gapsize (geo-standard-insets-top padding)
                                                               stickman caption))]
          [else ; just in case someone employs a quirk width
           (create-dia-block #:id id tags
                             #:intersect (if (< stickman-width width)
                                             (dia-apothem-intersect (abs (/ stickman-width width 2.0)) 0.5)
                                             dia-default-intersect)
                             #:with-group style (geo-vc-append #:desc (dia-block-desc-from-caption caption)
                                                               #:gapsize (geo-standard-insets-top padding)
                                                               stickman (geo-scale caption (/ width caption-width))))])))

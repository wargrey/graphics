#lang typed/racket/base

(provide (all-defined-out))

(require "../self.rkt")
(require "../convert.rkt")
(require "../paint.rkt")

(require "../unsafe/dc/stickman.rkt")
(require "../skeleton/stickman/self.rkt")
(require "../skeleton/stickman/interface.rkt")
(require "../skeleton/stickman/transform.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:stickman geo
  ([self : Geo-Standing-Stickman]
   [skeleton : Geo-Stickman-Skeleton]
   [scale : Nonnegative-Flonum]
   [fallback-thickness : Nonnegative-Flonum])
  #:type-name Geo:Stickman
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-stickman
  (lambda [#:id [id : (Option Symbol) #false]
           #:fallback-border-thickness [fallback-thickness : (Option Nonnegative-Flonum) 1.0]
           #:stroke [stroke : Maybe-Stroke-Paint #false]
           #:head-color [head-pattern : Maybe-Fill-Paint 'LawnGreen]
           #:body-color [body-pattern : Maybe-Fill-Paint 'LawnGreen]
           #:arm-color [arm-pattern : Maybe-Fill-Paint 'GhostWhite]
           [height : Real] [self : Geo-Standing-Stickman the-standing-stickman]] : Geo:Stickman
    (define skeleton : Geo-Stickman-Skeleton (geo-standing-stickman-snapshot self))
    (define fallback : Nonnegative-Flonum (or fallback-thickness 0.0))
    (define-values (flwidth flheight) (geo-standing-stickman-size skeleton fallback #:stroke (geo-select-stroke-paint stroke)))
    (define scale : Nonnegative-Flonum (abs (/ (real->double-flonum height) flheight)))
    
    (create-geometry-object geo:stickman
                            #:with [id (geo-draw-stickman stroke head-pattern body-pattern arm-pattern)
                                       (geo-shape-extent (* flwidth scale) (* flheight scale) 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            self skeleton scale fallback)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-draw-stickman : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Maybe-Fill-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke head-color body-color arm-color]
    (λ [self cr x0 y0 width height]
      (when (geo:stickman? self)
        (dc_stickman cr x0 y0 width height
                     (geo:stickman-skeleton self) (geo:stickman-scale self) (geo:stickman-fallback-thickness self)
                     (geo-select-stroke-paint alt-stroke) (geo-select-fill-source head-color)
                     (geo-select-fill-source body-color) (geo-select-fill-source arm-color))))))

#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/self)
(require geofun/digitama/paint)
(require geofun/digitama/paint/self)
(require geofun/digitama/unsafe/typed/cairo)

(require "self.rkt")
(require "../unsafe/dot.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-draw-curve : (-> Maybe-Stroke-Paint Boolean Geo-Surface-Draw!)
  (lambda [alt-stroke closed?]
    (λ [self cr x0 y0 width height]
      (when (plot:curve? self)
        (plot-draw-curve self cr x0 y0 width height alt-stroke closed?)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-draw-curve : (-> Plot:Curve
                              Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                              Maybe-Stroke-Paint Boolean
                              Void)
  (lambda [self cr x0 y0 width height alt-stroke closed?]
    (dc_line cr x0 y0 width height
             (geo:visualizer-position self) (plot:curve-dots self)
             (geo-select-stroke-paint* alt-stroke)
             closed?)))

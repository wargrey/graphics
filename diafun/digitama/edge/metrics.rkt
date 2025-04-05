#lang typed/racket/base

(provide (all-defined-out))

(require "tip.rkt")
(require "arrow.rkt")

(require geofun/digitama/geometry/footprint)
(require geofun/stroke)

(require racket/math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-dia-shape-stroke : Stroke (desc-stroke #:width 1.0 #:join 'round))

(define dia-edge-tip-metrics : (-> (Option Dia-Edge-Tip-Shape) Nonnegative-Flonum Flonum Float-Complex Boolean
                                   (Values Geo-Path-Clean-Prints Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum (Option Float-Complex) Boolean))
  (lambda [self 100% angle.rad dot forward?]
    (define-values (angle op)
      (if (not forward?)
          ;;; TODO: fix the tip which would be placed at the begining of edge
          (values (+ angle.rad pi) (λ [[offset : Float-Complex]] (- dot offset)))
          (values angle.rad        (λ [[offset : Float-Complex]] (- dot offset)))))
    
    (cond [(dia-arrow-tip? self) (dia-arrow-tip-vertices self 100% angle op)]
          [else (values null (real-part dot) (imag-part dot) 0.0 0.0 #false #true)])))

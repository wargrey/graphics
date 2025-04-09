#lang typed/racket/base

(provide (all-defined-out))

(require "tip.rkt")
(require "tip/arrow.rkt")
(require "tip/diamond.rkt")

(require geofun/digitama/geometry/footprint)

(require racket/math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-edge-tip-metrics : (-> (Option Dia-Edge-Tip-Shape) Nonnegative-Flonum Flonum Float-Complex Boolean
                                   (Values Geo-Path-Prints Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum (Option Float-Complex) Boolean))
  (lambda [self 100% angle.rad dot forward?]
    (define angle (if (not forward?) (+ angle.rad pi) angle.rad))
    
    (cond [(dia-arrow-tip? self) (dia-arrow-tip-vertices self 100% angle dot)]
          [(dia-diamond-tip? self) (dia-diamond-tip-vertices self 100% angle dot)]
          [else (values null (real-part dot) (imag-part dot) 0.0 0.0 #false #true)])))

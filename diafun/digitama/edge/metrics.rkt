#lang typed/racket/base

(provide (all-defined-out))

(require "tip.rkt")
(require "arrow.rkt")

(require geofun/digitama/geometry/footprint)
(require geofun/stroke)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-dia-shape-stroke : Stroke (desc-stroke #:width 1.0 #:join 'round))

(define dia-edge-tip-metrics : (-> (Option Dia-Edge-Tip-Shape) Nonnegative-Flonum Flonum Float-Complex
                                   (Values (Listof Geo-Path-Clean-Print) Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [self 100% angle.rad offset]
    (cond [(dia-arrow-tip? self) (dia-arrow-tip-vertices self 100% angle.rad offset)]
          [else (values null 0.0 0.0 0.0 0.0)])))
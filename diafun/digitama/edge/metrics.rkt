#lang typed/racket/base

(provide (all-defined-out))

(require "type.rkt")
(require "arrow.rkt")

(require geofun/digitama/geometry/footprint)
(require geofun/stroke)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-dia-shape-stroke : Stroke (desc-stroke #:width 1.0 #:join 'round))

(define dia-edge-shape-metrics : (-> (Option Dia-Edge-Shape) Nonnegative-Flonum Flonum Float-Complex
                                     (Values (Listof Geo-Path-Clean-Print) Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [self 100% angle.rad offset]
    (cond [(dia-edge-arrow? self) (dia-edge-arrow-vertices self 100% angle.rad offset)]
          [else (values null 0.0 0.0 0.0 0.0)])))

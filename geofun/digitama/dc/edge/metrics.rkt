#lang typed/racket/base

(provide (all-defined-out))

(require "type.rkt")
(require "arrow.rkt")

(require "../../geometry/footprint.rkt")
(require "../../../stroke.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-shape-stroke : Stroke (desc-stroke #:width 1.0 #:join 'round))

(define geo-edge-shape-metrics : (-> (Option Geo-Edge-Shape) Nonnegative-Flonum Flonum Float-Complex
                                     (Values (Listof Geo-Path-Clean-Print) Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [self 100% angle.rad offset]
    (cond [(geo-edge-arrow? self) (geo-edge-arrow-vertices self 100% angle.rad offset)]
          [else (values null 0.0 0.0 0.0 0.0)])))

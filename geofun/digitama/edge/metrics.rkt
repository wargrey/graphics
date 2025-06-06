#lang typed/racket/base

(provide (all-defined-out))

(require "../geometry/footprint.rkt")

(require "marker/self.rkt")
(require "marker/arrow.rkt")
(require "marker/diamond.rkt")

(require racket/math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-edge-marker-metrics : (-> (Option Geo-Marker) Nonnegative-Flonum Flonum Float-Complex Boolean Geo-Marker-Placement
                                      (Values Geo-Path-Prints Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Float-Complex Boolean))
  (lambda [self 100% angle.rad dot forward? position]
    (define angle (if (not forward?) (+ angle.rad pi) angle.rad))
    
    (define-values (shape x y w h offset fill?)
      (cond [(geo:mkr:arrow? self) (geo-arrow-vertices self 100% angle dot position)]
            [(geo:mkr:diamond? self) (geo-diamond-vertices self 100% angle dot position)]
            [else (values null (real-part dot) (imag-part dot) 0.0 0.0 0.0+0.0i #true)]))

    (values shape x y w h offset fill?)))

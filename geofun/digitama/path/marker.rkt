#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)

(require "../geometry/footprint.rkt")

(require "marker/self.rkt")

(require "marker/dot.rkt")
(require "marker/arrow.rkt")
(require "marker/diamond.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-marker-shape : (-> (Option geo-marker) Nonnegative-Flonum Flonum Boolean Geo-Tip-Placement
                               (Values Geo-Path-Prints
                                       Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                                       Float-Complex Geo-Marker-Config))
  (lambda [self 100% angle.rad forward? placement]
    (define angle (if (not forward?) (+ angle.rad pi) angle.rad))
    
    (cond [(geo:mrk:arrow? self)
           (geo-marker-values self (hash-ref! markerdb (list self 100% angle placement)
                                              (λ [] (geo-arrow-path self 100% angle placement))))]
          [(geo:mrk:dot? self)
           (geo-marker-values self (hash-ref! markerdb (list self 100% angle placement)
                                              (λ [] (geo-dot-path self 100% angle placement))))]
          [(geo:mrk:diamond? self)
           (geo-marker-values self (hash-ref! markerdb (list self 100% angle placement)
                                              (λ [] (geo-diamond-path self 100% angle placement))))]
          [else (values null 0.0 0.0 0.0 0.0 0.0+0.0i geo-filled-cfg)])))

(define geo-marker-shape-size : (-> (Option geo-marker) Nonnegative-Flonum Flonum (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [self 100% angle]
    (define-values (shape x0 y0 w h off cfg) (geo-marker-shape self 100% angle #false 'inside))
    (values w h)))

(define geo-marker-shape-height : (-> (Option geo-marker) Nonnegative-Flonum Flonum Nonnegative-Flonum)
  (lambda [self 100% angle]
    (define-values (shape x0 y0 w h off cfg) (geo-marker-shape self 100% angle #false 'inside))
    h))

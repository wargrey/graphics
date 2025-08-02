#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)

(require "../geometry/footprint.rkt")

(require "tip/self.rkt")

(require "tip/dot.rkt")
(require "tip/arrow.rkt")
(require "tip/diamond.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-tip-shape : (-> (Option geo-tip) Nonnegative-Flonum Flonum Boolean Geo-Tip-Placement
                            (Values Geo-Path-Prints
                                    Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                                    Float-Complex Geo-Tip-Config))
  (lambda [self 100% angle.rad forward? placement]
    (define angle (if (not forward?) (+ angle.rad pi) angle.rad))
    
    (cond [(geo:tip:arrow? self)
           (geo-tip-values self (hash-ref! tip-db (list self 100% angle placement)
                                           (λ [] (geo-arrow-path self 100% angle placement))))]
          [(geo:tip:dot? self)
           (geo-tip-values self (hash-ref! tip-db (list self 100% angle placement)
                                           (λ [] (geo-dot-path self 100% angle placement))))]
          [(geo:tip:diamond? self)
           (geo-tip-values self (hash-ref! tip-db (list self 100% angle placement)
                                           (λ [] (geo-diamond-path self 100% angle placement))))]
          [else (values null 0.0 0.0 0.0 0.0 0.0+0.0i geo-filled-cfg)])))

(define geo-tip-shape-size : (-> (Option geo-tip) Nonnegative-Flonum Flonum (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [self 100% angle]
    (define-values (shape x0 y0 w h off cfg) (geo-tip-shape self 100% angle #false 'inside))
    (values w h)))

(define geo-tip-shape-height : (-> (Option geo-tip) Nonnegative-Flonum Flonum Nonnegative-Flonum)
  (lambda [self 100% angle]
    (define-values (shape x0 y0 w h off cfg) (geo-tip-shape self 100% angle #false 'inside))
    h))

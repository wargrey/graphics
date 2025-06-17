#lang typed/racket/base

(provide (all-defined-out))

(require "../geometry/footprint.rkt")

(require "tip/self.rkt")
(require "tip/arrow.rkt")
(require "tip/diamond.rkt")

(require racket/math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-edge-tip-path : (-> (Option Geo-Tip) Nonnegative-Flonum Flonum Boolean Geo-Tip-Placement
                                (Values Geo-Path-Prints Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Float-Complex Boolean))
  (lambda [self 100% angle.rad forward? position]
    (define angle (if (not forward?) (+ angle.rad pi) angle.rad))
    
    (cond [(geo:tip:arrow? self)
           (geo-tip-values (hash-ref! tip-db (list self 100% angle position)
                                      (λ [] (geo-arrow-path self 100% angle position))))]
          [(geo:tip:diamond? self)
           (geo-tip-values (hash-ref! tip-db (list self 100% angle position)
                                      (λ [] (geo-diamond-path self 100% angle position))))]
          [else (values null 0.0 0.0 0.0 0.0 0.0+0.0i #true)])))

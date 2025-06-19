#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)

(require "../geometry/footprint.rkt")

(require "tip/self.rkt")

(require "tip/dot.rkt")
(require "tip/arrow.rkt")
(require "tip/diamond.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-tip-path : (-> (Option geo-tip) Nonnegative-Flonum Flonum Boolean Geo-Tip-Placement
                           (Values Geo-Path-Prints
                                   Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                                   Float-Complex Geo-Tip-Config))
  (lambda [self 100% angle.rad forward? position]
    (define angle (if (not forward?) (+ angle.rad pi) angle.rad))
    
    (cond [(geo:tip:arrow? self)
           (geo-tip-values self (hash-ref! tip-db (list self 100% angle position)
                                           (λ [] (geo-arrow-path self 100% angle position))))]
          [(geo:tip:dot? self)
           (geo-tip-values self (hash-ref! tip-db (list self 100% angle position)
                                           (λ [] (geo-dot-path self 100% angle position))))]
          [(geo:tip:diamond? self)
           (geo-tip-values self (hash-ref! tip-db (list self 100% angle position)
                                           (λ [] (geo-diamond-path self 100% angle position))))]
          [else (values null 0.0 0.0 0.0 0.0 0.0+0.0i geo-filled-cfg)])))

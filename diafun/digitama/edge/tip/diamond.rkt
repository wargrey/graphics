#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require digimon/metrics)

(require "../tip.rkt")

(require geofun/digitama/geometry/polygon/quadrilateral)
(require geofun/digitama/geometry/footprint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct dia-diamond-tip : Dia-Diamond-Tip #:-> Dia-Edge-Tip-Shape
  ([width : Real -8.0]
   [height : Real -6.0]
   [fill? : Boolean #true])
  #:transparent)

(define default-aggregation-tip : Dia-Diamond-Tip (make-dia-diamond-tip #:fill? #false))
(define default-composition-tip : Dia-Diamond-Tip (make-dia-diamond-tip #:fill? #true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-diamond-tip-vertices : (-> Dia-Diamond-Tip Nonnegative-Flonum Flonum Float-Complex
                                       (Values Geo-Path-Prints Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum (Option Float-Complex) Boolean))
  (lambda [self 100% angle.rad dot]
    (define w : Nonnegative-Flonum (~length (dia-diamond-tip-width self) 100%))
    (define h : Nonnegative-Flonum (~length (dia-diamond-tip-height self) 100%))
    (define offset : Float-Complex (- (make-polar w angle.rad)))
    (define origin : Float-Complex (- (make-polar (* w 0.5) angle.rad)))
    (define balanced : Float-Complex (make-rectangular (* w -0.5) (* h -0.5)))
    (define rhombus : Quadrilateral-Vertices (geo-rhombus-vertices w h angle.rad (+ dot origin balanced)))
    (define diamond : Geo-Path-Clean-Prints (geo-path-cleanse rhombus))
    (define-values (lx ty width height) (geo-path-ink-box diamond))

    (values (append diamond (list the-Z)) lx ty width height offset (dia-diamond-tip-fill? self))))

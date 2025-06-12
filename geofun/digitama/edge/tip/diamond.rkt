#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require digimon/metrics)

(require "self.rkt")

(require "../../geometry/polygon/quadrilateral.rkt")
(require "../../geometry/footprint.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct geo:tip:diamond : Geo:Tip:Diamond #:-> Geo-Tip
  ([width : Real -6.0]
   [height : Real -4.0]
   [fill? : Boolean #true])
  #:transparent)

(define default-aggregation-tip : Geo:Tip:Diamond (make-geo:tip:diamond #:fill? #false))
(define default-composition-tip : Geo:Tip:Diamond (make-geo:tip:diamond #:fill? #true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-diamond-vertices : (-> Geo:Tip:Diamond Nonnegative-Flonum Flonum Float-Complex Geo-Tip-Placement
                                   (Values Geo-Path-Prints Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Float-Complex Boolean))
  (lambda [self 100% angle.rad dot pos]
    (define-values (pos-wfrac pos-ofrac)
      (cond [(eq? pos 'inside) (values -1.0 -0.5)]
            [(eq? pos 'center) (values -0.5 +0.0)]
            [else (values 0.0 +0.5)]))
    
    (define w : Nonnegative-Flonum (~length (geo:tip:diamond-width self) 100%))
    (define h : Nonnegative-Flonum (~length (geo:tip:diamond-height self) 100%))
    (define offset : Float-Complex (+ (make-polar (* w pos-wfrac) angle.rad)))
    (define origin : Float-Complex (+ (make-polar (* w pos-ofrac) angle.rad)))
    (define balanced : Float-Complex (make-rectangular (* w -0.5) (* h -0.5)))
    (define rhombus : Quadrilateral-Vertices (geo-rhombus-vertices w h angle.rad (+ dot origin balanced)))
    (define diamond : Geo-Path-Clean-Prints (geo-path-cleanse rhombus))
    (define-values (lx ty width height) (geo-path-ink-box diamond))

    (values (append diamond (list the-Z)) lx ty width height offset (geo:tip:diamond-fill? self))))

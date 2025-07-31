#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require digimon/metrics)

(require "self.rkt")

(require "../../geometry/polygon/quadrilateral.rkt")
(require "../../geometry/footprint.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct geo:tip:diamond : Geo:Tip:Diamond #:-> geo-tip
  #:head ([geo-tip cfg : Geo-Tip-Config geo-filled-cfg])
  ([width : Real+% '(600 %)]
   [height : Real+% '(400 %)]
   [fill? : Boolean #true])
  #:transparent)

(define default-aggregation-tip : Geo:Tip:Diamond (make-geo:tip:diamond #:cfg geo-unfilled-cfg))
(define default-composition-tip : Geo:Tip:Diamond (make-geo:tip:diamond #:cfg geo-filled-cfg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-diamond-path : (-> Geo:Tip:Diamond Nonnegative-Flonum Flonum Geo-Tip-Placement Geo-Tip-Datum)
  (lambda [self 100% angle.rad pos]
    (define-values (pos-wfrac pos-ofrac)
      (cond [(eq? pos 'inside) (values -1.0 -0.5)]
            [(eq? pos 'center) (values -0.5 +0.0)]
            [else (values 0.0 +0.5)]))
    
    (define w : Nonnegative-Flonum (~length (geo:tip:diamond-width self) 100%))
    (define h : Nonnegative-Flonum (~length (geo:tip:diamond-height self) 100%))
    (define endpoint-offset : Float-Complex (+ (make-polar (* w pos-wfrac) angle.rad)))
    (define origin : Float-Complex (+ (make-polar (* w pos-ofrac) angle.rad)))
    (define balanced : Float-Complex (make-rectangular (* w -0.5) (* h -0.5)))
    (define rhombus : Quadrilateral-Vertices (geo-rhombus-vertices w h angle.rad (+ origin balanced)))
    (define diamond : Geo-Path-Clean-Prints (gpp-cleanse rhombus))
    (define-values (lx ty width height) (gpp-ink-box diamond))

    (vector-immutable (append diamond (list the-Z)) lx ty width height endpoint-offset)))

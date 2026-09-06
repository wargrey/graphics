#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require digimon/measure)

(require "self.rkt")

(require "../../geometry/polygon/quadrilateral.rkt")
(require "../../geometry/footprint.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct geo:mrk:diamond : Geo:Mrk:Diamond #:-> geo-marker
  #:head ([geo-marker cfg : Geo-Marker-Config geo-filled-cfg])
  ([width : Length+% (&% 600)]
   [height : Length+% (&% 400)]
   [fill? : Boolean #true])
  #:transparent)

(define the-aggregation.mrk : Geo:Mrk:Diamond (make-geo:mrk:diamond #:cfg geo-unfilled-cfg))
(define the-composition.mrk : Geo:Mrk:Diamond (make-geo:mrk:diamond #:cfg geo-filled-cfg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-diamond-path : (-> Geo:Mrk:Diamond Nonnegative-Flonum Flonum Geo-Tip-Placement Geo-Tip-Datum)
  (lambda [self 100% angle.rad pos]
    (define-values (pos-wfrac pos-ofrac)
      (cond [(eq? pos 'inside) (values -1.0 -0.5)]
            [(eq? pos 'center) (values -0.5 +0.0)]
            [else (values 0.0 +0.5)]))
    
    (define w : Nonnegative-Flonum (~dimension (geo:mrk:diamond-width self) 100%))
    (define h : Nonnegative-Flonum (~dimension (geo:mrk:diamond-height self) 100%))
    (define endpoint-offset : Float-Complex (+ (make-polar (* w pos-wfrac) angle.rad)))
    (define origin : Float-Complex (+ (make-polar (* w pos-ofrac) angle.rad)))
    (define balanced : Float-Complex (make-rectangular (* w -0.5) (* h -0.5)))
    (define rhombus : Quadrilateral-Vertices (geo-rhombus-vertices w h angle.rad (+ origin balanced)))
    (define diamond : Geo-Path-Clean-Prints (gpp-cleanse rhombus))
    (define-values (lx ty width height) (gpp-ink-box diamond))

    (vector-immutable (append diamond (list the-Z)) lx ty width height endpoint-offset)))

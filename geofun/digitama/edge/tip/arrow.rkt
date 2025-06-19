#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require digimon/metrics)

(require "self.rkt")

(require geofun/digitama/geometry/polygon/arrow)
(require geofun/digitama/geometry/footprint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct geo:tip:arrow : Geo:Tip:Arrow #:-> geo-tip
  #:head ([geo-tip cfg : Geo-Tip-Config geo-filled-cfg])
  ([radius : Real -3.0]
   [wing.deg : (Option Real) #false]
   [curved? : Boolean #true])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-arrow-tip : Geo:Tip:Arrow (make-geo:tip:arrow))
(define default-generalization-tip : Geo:Tip:Arrow
  (make-geo:tip:arrow #:radius -3.5 #:wing.deg 180.0 #:curved? #false #:cfg geo-unfilled-cfg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-arrow-path : (-> Geo:Tip:Arrow Nonnegative-Flonum Flonum Geo-Tip-Placement Geo-Tip-Datum)
  (lambda [self 100% angle.rad pos]
    (define-values (pos-rfrac pos-afrac)
      (cond [(eq? pos 'inside) (values -1.0 -0.5)]
            [(eq? pos 'center) (values -0.5 0.25)]
            [else (values +0.5 0.0)]))
    
    (define r : Nonnegative-Flonum (~length (geo:tip:arrow-radius self) 100%))
    (define wing : (Option Real) (geo:tip:arrow-wing.deg self))
    (define endpoint-offset : Float-Complex
      (+ (make-polar (* r pos-rfrac) angle.rad)
         (make-polar (* 100% pos-afrac) angle.rad)))
    
    (define-values (arrow _x _y _w _h)
      (if (geo:tip:arrow-curved? self)
          (geo-curved-dart-metrics r angle.rad (and wing (~radian wing)) endpoint-offset)
          (geo-dart-metrics r angle.rad (and wing (~radian wing)) endpoint-offset)))
    (define-values (lx ty width height) (geo-path-ink-box arrow))

    (vector-immutable arrow lx ty width height endpoint-offset)))

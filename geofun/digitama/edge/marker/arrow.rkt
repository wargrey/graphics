#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require digimon/metrics)

(require "self.rkt")

(require geofun/digitama/geometry/polygon/arrow)
(require geofun/digitama/geometry/footprint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct geo:mkr:arrow : Geo:Mkr:Arrow #:-> Geo-Marker
  ([radius : Real -3.0]
   [wing.deg : (Option Real) #false]
   [curved? : Boolean #true]
   [fill? : Boolean #true])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-arrow-marker : Geo-Marker (make-geo:mkr:arrow))
(define default-generalization-marker : Geo-Marker (make-geo:mkr:arrow #:radius -3.5 #:wing.deg 180.0 #:curved? #false #:fill? #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-arrow-vertices : (-> Geo:Mkr:Arrow Nonnegative-Flonum Flonum Float-Complex Geo-Marker-Placement
                                 (Values Geo-Path-Prints Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Float-Complex Boolean))
  (lambda [self 100% angle.rad dot pos]
    (define-values (pos-rfrac pos-afrac)
      (cond [(eq? pos 'inside) (values -1.0 -0.5)]
            [(eq? pos 'center) (values -0.5 0.25)]
            [else (values +0.5 0.0)]))
    
    (define fill? : Boolean (geo:mkr:arrow-fill? self))
    (define r : Nonnegative-Flonum (~length (geo:mkr:arrow-radius self) 100%))
    (define wing : (Option Real) (geo:mkr:arrow-wing.deg self))
    (define offset : Float-Complex (+ (make-polar (* r pos-rfrac) angle.rad) (make-polar (* 100% pos-afrac) angle.rad)))
    (define-values (arrow _x _y _w _h)
      (if (geo:mkr:arrow-curved? self)
          (geo-curved-dart-metrics r angle.rad (and wing (~radian wing)) (+ dot offset))
          (geo-dart-metrics r angle.rad (and wing (~radian wing)) (+ dot offset))))
    (define-values (lx ty width height) (geo-path-ink-box arrow))

    (values arrow lx ty width height offset fill?)))

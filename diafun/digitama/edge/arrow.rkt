#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require digimon/metrics)

(require "type.rkt")

(require geofun/digitama/geometry/polygon/arrow)
(require geofun/digitama/geometry/footprint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct dia-edge-arrow : Dia-Edge-Arrow #:-> Dia-Edge-Shape
  ([radius : Real -2.5]
   [wing.deg : (Option Real) #false]
   [t : Nonnegative-Flonum 0.92])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-edge-arrow-vertices : (-> Dia-Edge-Arrow Nonnegative-Flonum Flonum Float-Complex
                                      (Values (Listof Geo-Path-Clean-Print) Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [self 100% angle.rad dot]
    (define r : Nonnegative-Flonum (~length (dia-edge-arrow-radius self) 100%))
    (define wing : (Option Real) (dia-edge-arrow-wing.deg self))
    (define offset : Float-Complex (make-polar (* r (dia-edge-arrow-t self)) angle.rad))
    (define-values (prints _x _y _w _h) (geo-dart-metrics r angle.rad (and wing (~radian wing)) (- dot offset)))
    (define-values (lx ty width height) (geo-path-ink-box prints))
    
    (values prints lx ty width height)))

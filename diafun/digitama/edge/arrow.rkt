#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require digimon/metrics)

(require "tip.rkt")

(require geofun/digitama/geometry/polygon/arrow)
(require geofun/digitama/geometry/footprint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct dia-arrow-tip : Dia-Arrow-Tip #:-> Dia-Edge-Tip-Shape
  ([radius : Real -3.0]
   [wing.deg : (Option Real) #false]
   [t : Nonnegative-Flonum 1.00])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-arrow-tip-vertices : (-> Dia-Arrow-Tip Nonnegative-Flonum Flonum Float-Complex
                                     (Values (Listof Geo-Path-Clean-Print) Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum (Option Float-Complex)))
  (lambda [self 100% angle.rad dot]
    (define r : Nonnegative-Flonum (~length (dia-arrow-tip-radius self) 100%))
    (define wing : (Option Real) (dia-arrow-tip-wing.deg self))
    (define offset : Float-Complex (make-polar (* r (dia-arrow-tip-t self)) angle.rad))
    (define-values (prints _x _y _w _h) (geo-curved-dart-metrics r angle.rad (and wing (~radian wing)) (- dot offset)))
    (define-values (lx ty width height) (geo-path-ink-box prints))
    
    (values prints lx ty width height offset)))

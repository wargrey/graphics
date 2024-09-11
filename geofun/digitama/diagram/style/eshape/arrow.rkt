#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require digimon/metrics)
(require digimon/constant)

(require "type.rkt")

(require "../../../geometry/polygon/arrow.rkt")
(require "../../../geometry/footprint.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct geo-edge-arrow : Geo-Edge-Arrow #:-> Geo-Edge-Shape
  ([radius : Real -2.5]
   [wing.deg : (Option Real) #false])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-edge-arrow-vertices : (-> Geo-Edge-Arrow Nonnegative-Flonum Flonum Float-Complex
                                      (Values (Listof Geo-Path-Clean-Print) Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [self 100% angle.rad dot]
    (define r : Nonnegative-Flonum (~length (geo-edge-arrow-radius self) 100%))
    (define wing : (Option Real) (geo-edge-arrow-wing.deg self))
    (define offset : Float-Complex (make-polar r angle.rad))
    (define-values (prints _x _y _w _h) (geo-dart-metrics r angle.rad (and wing (~radian wing)) (- dot offset)))
    (define-values (lx ty width height) (geo-path-ink-box prints))
    
    (values prints lx ty width height)))

#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require digimon/metrics)

(require "../tip.rkt")

(require geofun/digitama/geometry/polygon/arrow)
(require geofun/digitama/geometry/footprint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct dia-arrow-tip : Dia-Arrow-Tip #:-> Dia-Edge-Tip-Shape
  ([radius : Real -3.0]
   [wing.deg : (Option Real) #false]
   [curved? : Boolean #true]
   [fill? : Boolean #true])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-arrow-tip : Dia-Edge-Tip-Shape (make-dia-arrow-tip))
(define default-generalization-tip : Dia-Edge-Tip-Shape (make-dia-arrow-tip #:radius -5.0 #:wing.deg 180.0 #:curved? #false #:fill? #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-arrow-tip-vertices : (-> Dia-Arrow-Tip Nonnegative-Flonum Flonum Float-Complex
                                     (Values Geo-Path-Prints Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum (Option Float-Complex) Boolean))
  (lambda [self 100% angle.rad dot]
    (define r : Nonnegative-Flonum (~length (dia-arrow-tip-radius self) 100%))
    (define wing : (Option Real) (dia-arrow-tip-wing.deg self))
    (define offset : Float-Complex (- (make-polar r angle.rad)))
    (define-values (arrow _x _y _w _h)
      (if (dia-arrow-tip-curved? self)
          (geo-curved-dart-metrics r angle.rad (and wing (~radian wing)) (+ dot offset))
          (geo-dart-metrics r angle.rad (and wing (~radian wing)) (+ dot offset))))
    (define-values (lx ty width height) (geo-path-ink-box arrow))

    (values arrow lx ty width height offset (dia-arrow-tip-fill? self))))

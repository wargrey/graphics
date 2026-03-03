#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)

(require digimon/struct)
(require digimon/measure)

(require "self.rkt")

(require geofun/digitama/geometry/polygon/arrow)
(require geofun/digitama/geometry/footprint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct geo:tip:arrow : Geo:Tip:Arrow #:-> geo-tip
  #:head ([geo-tip cfg : Geo-Tip-Config geo-filled-cfg])
  ([radius : Length+% (&% 400)]
   [wing-angle : (Option Real) #false]
   [curved? : (U Boolean Real) #true])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-arrow-tip : Geo:Tip:Arrow (make-geo:tip:arrow))
(define default-generalization-tip : Geo:Tip:Arrow
  (make-geo:tip:arrow #:radius (&% 350) #:wing-angle pi #:curved? #false #:cfg geo-unfilled-cfg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-arrow-path : (-> Geo:Tip:Arrow Nonnegative-Flonum Flonum Geo-Tip-Placement Geo-Tip-Datum)
  (lambda [self 100% angle.rad pos]
    (define-values (pos-rfrac pos-afrac)
      (cond [(eq? pos 'inside) (values -1.0 -0.5)]
            [(eq? pos 'center) (values -0.5 0.25)]
            [else (values +0.5 0.0)]))
    
    (define r : Nonnegative-Flonum (~dimension (geo:tip:arrow-radius self) 100%))
    (define wing : (Option Real) (geo:tip:arrow-wing-angle self))
    (define endpoint-offset : Float-Complex
      (+ (make-polar (* r pos-rfrac) angle.rad)
         (make-polar (* 100% pos-afrac) angle.rad)))

    (define curved? (geo:tip:arrow-curved? self))
    (define wing.rad (and wing (real->double-flonum wing)))
    (define-values (arrow _x _y _w _h)
      (cond [(not curved?) (geo-dart-metrics r angle.rad wing.rad endpoint-offset)]
            [(boolean? curved?) (geo-curved-dart-metrics r angle.rad wing.rad endpoint-offset)]
            [else (let ([t (real->double-flonum curved?)])
                    (if (<= 0.0 t 1.0)
                        (geo-curved-dart-metrics r angle.rad wing.rad endpoint-offset t)
                        (geo-curved-dart-metrics r angle.rad wing.rad endpoint-offset)))]))
    (define-values (lx ty width height) (gpp-ink-box arrow))

    (vector-immutable arrow lx ty width height endpoint-offset)))

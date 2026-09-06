#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require digimon/measure)

(require "self.rkt")

(require geofun/digitama/geometry/polygon/arrow)
(require geofun/digitama/geometry/footprint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct geo:mrk:arrow : Geo:Mrk:Arrow #:-> geo-marker
  #:head ([geo-marker cfg : Geo-Marker-Config geo-filled-cfg])
  ([radius : Length+% (&% 400)]
   [wing-angle : (Option Real) #false]
   [curved? : (U Boolean Real) #true])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-arrow.mrk : Geo:Mrk:Arrow (make-geo:mrk:arrow))
(define the-generalization.mrk : Geo:Mrk:Arrow
  (make-geo:mrk:arrow #:radius (&% 350) #:wing-angle pi #:curved? #false #:cfg geo-unfilled-cfg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-arrow-path : (-> Geo:Mrk:Arrow Nonnegative-Flonum Flonum Geo-Tip-Placement Geo-Tip-Datum)
  (lambda [self 100% angle.rad pos]
    (define-values (pos-rfrac pos-afrac)
      (cond [(eq? pos 'inside) (values -1.0 -0.5)]
            [(eq? pos 'center) (values -0.5 0.25)]
            [else (values +0.5 0.0)]))
    
    (define r : Nonnegative-Flonum (~dimension (geo:mrk:arrow-radius self) 100%))
    (define wing : (Option Real) (geo:mrk:arrow-wing-angle self))
    (define endpoint-offset : Float-Complex
      (+ (make-polar (* r pos-rfrac) angle.rad)
         (make-polar (* 100% pos-afrac) angle.rad)))

    (define curved? (geo:mrk:arrow-curved? self))
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

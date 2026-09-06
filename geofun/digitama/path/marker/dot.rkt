#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require digimon/measure)

(require "self.rkt")

(require geofun/digitama/geometry/footprint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct geo:mrk:dot : Geo:Mrk:Dot #:-> geo-marker
  #:head ([geo-marker cfg : Geo-Marker-Config geo-filled-cfg])
  ([radius : Length+% (&% 150)])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-dot.mrk : Geo:Mrk:Dot (make-geo:mrk:dot #:radius 1.5))
(define the-odot.mrk : Geo:Mrk:Dot (make-geo:mrk:dot #:radius 1.5 #:cfg geo-hollow-cfg))
(define the-pixel.mrk : Geo:Mrk:Dot (make-geo:mrk:dot #:radius 0.5))
(define the-bullet.mrk : Geo:Mrk:Dot (make-geo:mrk:dot))
(define the-circle.mrk : Geo:Mrk:Dot (make-geo:mrk:dot #:cfg geo-hollow-cfg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-dot-path : (-> Geo:Mrk:Dot Nonnegative-Flonum Flonum Geo-Tip-Placement Geo-Tip-Datum)
  (lambda [self 100% angle.rad pos]
    (define-values (pos-rfrac pos-afrac)
      (cond [(eq? pos 'inside)  (values -2.0 -0.5)]
            [(eq? pos 'outside) (values +0.0 +0.5)]
            [else (values -1.0 0.0)]))
    
    (define r : Nonnegative-Flonum (~dimension (geo:mrk:dot-radius self) 100%))
    (define size : Nonnegative-Flonum (* (+ r 1.0) 2.0))
    (define endpoint-offset : Float-Complex
      (+ (make-polar (* r pos-rfrac) angle.rad)
         (make-polar (* 100% pos-afrac) angle.rad)))
    (define center : Float-Complex (+ (make-polar r angle.rad) endpoint-offset))

    (vector-immutable (list (gpp:arc #\A 0.0+0.0i center r r 0.0 2pi #true))
                      (* size -0.5) (* size -0.5) size size endpoint-offset)))

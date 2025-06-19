#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require digimon/metrics)
(require digimon/constant)

(require "self.rkt")

(require geofun/digitama/geometry/footprint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct geo:tip:dot : Geo:Tip:Dot #:-> geo-tip
  #:head ([geo-tip cfg : Geo-Tip-Config geo-filled-cfg])
  ([radius : Real -1.5])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-dot-tip : Geo:Tip:Dot (make-geo:tip:dot #:radius 1.5))
(define default-odot-tip : Geo:Tip:Dot (make-geo:tip:dot #:radius 1.5 #:cfg geo-hollow-cfg))
(define default-pixel-tip : Geo:Tip:Dot (make-geo:tip:dot #:radius 0.5))
(define default-bullet-tip : Geo:Tip:Dot (make-geo:tip:dot))
(define default-circle-tip : Geo:Tip:Dot (make-geo:tip:dot #:cfg geo-hollow-cfg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-dot-path : (-> Geo:Tip:Dot Nonnegative-Flonum Flonum Geo-Tip-Placement Geo-Tip-Datum)
  (lambda [self 100% angle.rad pos]
    (define-values (pos-rfrac pos-afrac)
      (cond [(eq? pos 'inside)  (values -2.0 -0.5)]
            [(eq? pos 'outside) (values +0.0 +0.5)]
            [else (values -1.0 0.0)]))
    
    (define r : Nonnegative-Flonum (~length (geo:tip:dot-radius self) 100%))
    (define size : Nonnegative-Flonum (* r 2.0))
    (define endpoint-offset : Float-Complex
      (+ (make-polar (* r pos-rfrac) angle.rad)
         (make-polar (* 100% pos-afrac) angle.rad)))
    (define center : Float-Complex (+ (make-polar r angle.rad) endpoint-offset))

    (vector-immutable (list (gpp:arc #\A 0.0+0.0i center r r 0.0 2pi #true))
                      (- r) (- r) size size endpoint-offset)))

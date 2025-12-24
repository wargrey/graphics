#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)
(require digimon/constant)

(require "../self.rkt")
(require "../convert.rkt")

(require "../paint.rkt")
(require "../unsafe/dc/shape.rkt")
(require "../unsafe/dc/gear.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:gear geo
  ([teeth-number : Index]
   [pressure-angle : Flonum]
   [radius : Nonnegative-Flonum]
   [root-radius : Nonnegative-Flonum]
   [reference-ratio : Nonnegative-Flonum]
   [inner-radius : Nonnegative-Flonum]
   [rotation : Flonum])
  #:type-name Geo:Gear
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-gear
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           #:id [id : (Option Symbol) #false]
           #:pressure-angle [angle : Real (/ pi 9.0)]
           #:root-radius [root-radius : Real+% '(81 %)]
           #:inner-radius [inner-radius : Real+% '(61.8 %)]
           #:reference-ratio [ref-ratio : Nonnegative-Real 5/9]
           [n : Integer] [tip-radius : Real] [rotation : Real 0.0]] : Geo:Gear
    (define Rt : Nonnegative-Flonum (~length tip-radius))
    (define Rr : Nonnegative-Flonum (~length root-radius Rt))
    (define Ri : Nonnegative-Flonum (~length inner-radius Rt))
    (define α : Flonum (real->double-flonum angle))
    (define z : Index (if (index? n) n 0))
    (define d : Nonnegative-Flonum (* 2.0 Rt))
    
    (create-geometry-object geo:gear
                            #:with [id (geo-draw-gear stroke pattern)
                                       (geo-shape-extent d d 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            z α Rt Rr (~clamp ref-ratio 0.0 1.0)
                            Ri (real->double-flonum rotation))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-draw-gear : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill]
    (λ [self cr x0 y0 width height]
      (when (geo:gear? self)
        (define z (geo:gear-teeth-number self))
        (define Rt (geo:gear-radius self))
        (define Rr (/ (geo:gear-root-radius self) Rt))
        (define Ri (/ (geo:gear-inner-radius self) Rt))
        
        (if (and (> z 0) (< Ri Rr 1.0) (> Rr 0.0) (>= Ri 0.0))
            (dc_gear cr x0 y0 width height z (geo:gear-pressure-angle self) Rr (geo:gear-reference-ratio self)
                     Ri (geo:gear-rotation self) (geo-select-stroke-paint alt-stroke)
                     (geo-select-fill-source alt-fill))
            (dc_ellipse cr x0 y0 width height
                        (geo-select-stroke-paint alt-stroke) (geo-select-fill-source alt-fill)
                        null))))))

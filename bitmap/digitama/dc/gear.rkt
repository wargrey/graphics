#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)
(require digimon/constant)

(require geofun/paint)

(require geofun/digitama/base)
(require geofun/digitama/paint/source)
(require geofun/digitama/unsafe/dc/shape)
(require geofun/digitama/unsafe/dc/gear)

(require "../convert.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-gear
  (lambda [#:stroke [outline : Maybe-Stroke-Paint (default-stroke-paint)] #:fill [pattern : Option-Fill-Paint (default-fill-paint)]
           #:radian? [radian? : Boolean #true] #:pressure-angle [angle : Real (/ pi 9.0)] #:reference-ratio [ref-ratio : Nonnegative-Real 5/9]
           #:root-radius [root-radius : Real -0.81] #:inner-radius [inner-radius : Real -0.618] #:density [density : Positive-Flonum (default-bitmap-density)]
           [n : Integer] [tip-radius : Real] [rotation : Real 0.0]] : Bitmap
    (define Rt : Nonnegative-Flonum (~length tip-radius))
    (define Rr : Flonum (/ (~length root-radius  Rt) Rt))
    (define Ri : Flonum (/ (~length inner-radius Rt) Rt))
    (define α : Flonum (~radian angle radian?))
    (define z : Index (if (index? n) n 0))
    (define d : Nonnegative-Flonum (* 2.0 Rt))

    (if (and (> z 0) (< Ri Rr 1.0) (> Rr 0.0) (>= Ri 0.0))
        (draw-bitmap dc_gear #:with [d d density #true (stroke-paint->source* outline)]
                     [z α Rr (~clamp ref-ratio 0.0 1.0) Ri (~radian rotation radian?)]
                     [(fill-paint->source* pattern)])
        (draw-bitmap dc_ellipse #:with [d d density #true (stroke-paint->source* outline)]
                     [] [(fill-paint->source* pattern) null]))))

#lang typed/racket/base

(provide (all-defined-out) 3D-Radius-Type)

(require "digitama/base.rkt")
(require "digitama/source.rkt")

(require "digitama/unsafe/convert.rkt")
(require "digitama/unsafe/icosahedron.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-icosahedron-side-projection : (->* (Real)
                                                  (3D-Radius-Type #:edge (Option Stroke-Paint) #:border (Option Stroke-Paint) #:fill (Option Fill-Paint)
                                                                  #:density Positive-Flonum)
                                                  Bitmap)
  (lambda [#:edge [edge (default-stroke)] #:border [border #false] #:fill [pattern #false] #:density [density (default-bitmap-density)]
           radius [radius-type 'vertex]]
    (bitmap_icosahedron_side_proj (real->double-flonum radius) radius-type
                                  (stroke-paint->source* edge) (fill-paint->source* pattern) (stroke-paint->source* border)
                                  density)))

(define bitmap-icosahedron-over-projection : (->* (Real)
                                                  (3D-Radius-Type #:edge (Option Stroke-Paint) #:border (Option Stroke-Paint) #:fill (Option Fill-Paint)
                                                                  #:rotation Real #:radian? Boolean #:density Positive-Flonum)
                                                  Bitmap)
  (lambda [#:edge [edge (default-stroke)] #:border [border #false] #:fill [pattern #false]
           #:rotation [rotation 0.0] #:radian? [radian? #true] #:density [density (default-bitmap-density)]
           radius [radius-type 'vertex]]
    (bitmap_icosahedron_over_proj (real->double-flonum radius) radius-type (real->double-flonum rotation)
                                  (stroke-paint->source* edge) (fill-paint->source* pattern) (stroke-paint->source* border)
                                  radian? density)))

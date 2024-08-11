#lang typed/racket/base

(provide (all-defined-out) 3D-Radius-Type)

(require "digitama/convert.rkt")

(require digimon/metrics)

(require geofun/paint)
(require geofun/digitama/base)
(require geofun/digitama/source)
(require geofun/digitama/unsafe/dc/icosahedron)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-icosahedron-side-projection : (->* (Real)
                                                  (3D-Radius-Type #:edge Maybe-Stroke-Paint #:border Maybe-Stroke-Paint #:fill Maybe-Fill-Paint
                                                                  #:density Positive-Flonum)
                                                  Bitmap)
  (lambda [#:edge [edge (default-stroke-paint)] #:border [border #false] #:fill [pattern #false] #:density [density (default-bitmap-density)]
           radius [radius-type 'vertex]]
    (dc_icosahedron_side_proj create-argb-bitmap
                              (~length radius) radius-type
                              (stroke-paint->source* edge) (fill-paint->source* pattern) (stroke-paint->source* border)
                              density)))

(define bitmap-icosahedron-over-projection : (->* (Real)
                                                  (3D-Radius-Type #:edge Maybe-Stroke-Paint #:border Maybe-Stroke-Paint #:fill Maybe-Fill-Paint
                                                                  #:rotation Real #:radian? Boolean #:density Positive-Flonum)
                                                  Bitmap)
  (lambda [#:edge [edge (default-stroke-paint)] #:border [border #false] #:fill [pattern #false]
           #:rotation [rotation 0.0] #:radian? [radian? #true] #:density [density (default-bitmap-density)]
           radius [radius-type 'vertex]]
    (dc_icosahedron_over_proj create-argb-bitmap
                              (~length radius) radius-type (~radian rotation radian?)
                              (stroke-paint->source* edge) (fill-paint->source* pattern) (stroke-paint->source* border)
                              density)))

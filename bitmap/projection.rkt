#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/convert.rkt")

(require digimon/metrics)

(require geofun/paint)
(require geofun/stroke)
(require geofun/digitama/base)
(require geofun/digitama/source)
(require geofun/digitama/unsafe/dc/icosahedron)
(require geofun/digitama/geometry/radius)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-icosahedron-side-projection : (->* (Real)
                                                  (3D-Radius-Type #:edge Option-Stroke-Paint #:border Option-Stroke-Paint #:fill Option-Fill-Paint
                                                                  #:density Positive-Flonum)
                                                  Bitmap)
  (lambda [#:edge [edge (default-stroke-paint)] #:border [border #false] #:fill [pattern #false] #:density [density (default-bitmap-density)]
           radius [radius-type 'vertex]]
    (define sidelength : Nonnegative-Flonum (icosahedron-radius->edge-length (~length radius) radius-type))
    (define fllength : Nonnegative-Flonum (icosahedron-edge-length->side-outline-size sidelength))
    (define-values (es bs) (values (stroke-paint->source* edge) (stroke-paint->source* border)))
    
    (draw-bitmap dc_icosahedron_side_proj #:with [fllength fllength density #true (or bs es)]
                 [es (fill-paint->source* pattern)] [])))

(define bitmap-icosahedron-over-projection : (->* (Real)
                                                  (3D-Radius-Type #:edge Option-Stroke-Paint #:border Option-Stroke-Paint #:fill Option-Fill-Paint
                                                                  #:rotation Real #:radian? Boolean #:density Positive-Flonum)
                                                  Bitmap)
  (lambda [#:edge [edge (default-stroke-paint)] #:border [border #false] #:fill [pattern #false]
           #:rotation [rotation 0.0] #:radian? [radian? #true] #:density [density (default-bitmap-density)]
           radius [radius-type 'vertex]]
    (define R : Nonnegative-Flonum (icosahedron-radius->circumsphere-radius (~length radius) radius-type))
    (define fllength : Nonnegative-Flonum (* R 0.5))
    (define-values (es bs) (values (stroke-paint->source* edge) (stroke-paint->source* border)))
    
    (draw-bitmap dc_icosahedron_over_proj #:with [fllength fllength density #true (or bs es)]
                 [(~radian rotation radian?) es (fill-paint->source* pattern)] [])))

#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/convert.rkt")

(require digimon/metrics)

(require geofun/paint)
(require geofun/digitama/base)
(require geofun/digitama/paint/source)
(require geofun/digitama/geometry/radius)

(require geofun/digitama/unsafe/dc/icosahedron)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-icosahedron-side-projection
  (lambda [#:edge [edge : Maybe-Stroke-Paint (default-stroke-paint)]
           #:border [border : Option-Stroke-Paint #false]
           #:fill [pattern : Option-Fill-Paint #false]
           #:density [density : Positive-Flonum (default-bitmap-density)]
           [radius : Real] [radius-type : 3D-Radius-Type 'vertex]] : Bitmap
    (define sidelength : Nonnegative-Flonum (icosahedron-radius->edge-length (~length radius) radius-type))
    (define fllength : Nonnegative-Flonum (icosahedron-edge-length->side-outline-size sidelength))
    (define-values (es bs) (values (stroke-paint->source* edge) (stroke-paint->source* border)))
    
    (draw-bitmap dc_icosahedron_side_proj #:with [fllength fllength density #true (or bs es)]
                 [es (fill-paint->source* pattern)] [])))

(define bitmap-icosahedron-over-projection
  (lambda [#:edge [edge : Maybe-Stroke-Paint (default-stroke-paint)]
           #:border [border : Option-Stroke-Paint #false]
           #:fill [pattern : Option-Fill-Paint #false]
           #:rotation [rotation : Real 0.0]
           #:density [density : Positive-Flonum (default-bitmap-density)]
           [radius : Real] [radius-type : 3D-Radius-Type 'vertex]] : Bitmap
    (define R : Nonnegative-Flonum (icosahedron-radius->circumsphere-radius (~length radius) radius-type))
    (define fllength : Nonnegative-Flonum (* R 0.5))
    (define-values (es bs) (values (stroke-paint->source* edge) (stroke-paint->source* border)))
    
    (draw-bitmap dc_icosahedron_over_proj #:with [fllength fllength density #true (or bs es)]
                 [(real->double-flonum rotation) es (fill-paint->source* pattern)] [])))

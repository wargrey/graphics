#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require "../paint.rkt")
(require "../../paint.rkt")

(require "../convert.rkt")
(require "../geometry/radius.rkt")
(require "../unsafe/dc/icosahedron.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:icosahedron:side geo
  ([radius : Nonnegative-Flonum]
   [radius-type : 3D-Radius-Type]
   [edge-length : Nonnegative-Flonum])
  #:type-name Geo:Icosahedron:Side
  #:transparent)

(struct geo:icosahedron:over geo
  ([radius : Nonnegative-Flonum]
   [radius-type : 3D-Radius-Type]
   [circumsphere-radius : Nonnegative-Flonum]
   [rotation : Flonum])
  #:type-name Geo:Icosahedron:Over
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-icosahedron-side-projection
  (lambda [#:edge [edge : Maybe-Stroke-Paint (void)] #:border [border : Maybe-Stroke-Paint (void)] #:fill [pattern : Maybe-Fill-Paint (void)]
           #:id [id : (Option Symbol) #false]
           [radius : Real] [radius-type : 3D-Radius-Type 'vertex]] : Geo:Icosahedron:Side
    (define r : Nonnegative-Flonum (~length radius))
    (define sidelength : Nonnegative-Flonum (icosahedron-radius->edge-length r radius-type))
    
    (create-geometry-object geo:icosahedron:side
                            #:with [id (geo-draw-side-projection border edge pattern)
                                       (geo-shape-extent (icosahedron-edge-length->side-outline-size sidelength))
                                       (geo-side-projection-outline edge border)]
                            r radius-type sidelength)))

(define geo-icosahedron-over-projection
  (lambda [#:edge [edge : Maybe-Stroke-Paint (void)] #:border [border : Maybe-Stroke-Paint (void)] #:fill [pattern : Maybe-Fill-Paint (void)]
           #:id [id : (Option Symbol) #false] #:rotation [rotation : Real 0.0] #:radian? [radian? : Boolean #true]
           [radius : Real] [radius-type : 3D-Radius-Type 'vertex]] : Geo:Icosahedron:Over
    (define r : Nonnegative-Flonum (~length radius))
    (define R : Nonnegative-Flonum (icosahedron-radius->circumsphere-radius r radius-type))
    
    (create-geometry-object geo:icosahedron:over
                            #:with [id (geo-draw-over-projection border edge pattern)
                                       (geo-shape-extent (* 2.0 R))
                                       (geo-side-projection-outline edge border)]
                            r radius-type R (~radian rotation radian?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-draw-side-projection : (-> Maybe-Stroke-Paint Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-bdr alt-edge alt-fill]
    (λ [self cr x0 y0 width height]
      (when (geo:icosahedron:side? self)
        (dc_icosahedron_side_proj cr x0 y0 width height
                                  (geo-select-stroke-paint alt-edge) (geo-select-fill-source alt-fill)
                                  (geo-select-border-paint alt-bdr))))))

(define geo-draw-over-projection : (-> Maybe-Stroke-Paint Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-bdr alt-edge alt-fill]
    (λ [self cr x0 y0 width height]
      (when (geo:icosahedron:over? self)
        (dc_icosahedron_over_proj cr x0 y0 width height
                                  (geo:icosahedron:over-rotation self)
                                  (geo-select-stroke-paint alt-edge) (geo-select-fill-source alt-fill)
                                  (geo-select-border-paint alt-bdr))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-side-projection-outline : (-> Maybe-Stroke-Paint Maybe-Stroke-Paint Geo-Calculate-Outline)
  (lambda [alt-edge alt-bdr]
    (λ [self cur-edge cur-bdr]
      (define edge (if (void? alt-edge) cur-edge alt-edge))
      (define border (if (void? alt-bdr) cur-bdr alt-bdr))
      
      (or (geo-shape-outline (or border edge))
          geo-zero-pads))))

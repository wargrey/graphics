#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require "paint.rkt")
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
(define geo-icosahedron-side-projection : (->* (Real)
                                               (3D-Radius-Type #:id (Option Symbol)
                                                               #:edge Maybe-Stroke-Paint #:border Maybe-Stroke-Paint #:fill Maybe-Fill-Paint)
                                               Geo:Icosahedron:Side)
  (lambda [#:id [id #false] #:edge [edge (void)] #:border [border (void)] #:fill [pattern (void)]
           radius [radius-type 'vertex]]
    (define r : Nonnegative-Flonum (~length radius))
    (define sidelength : Nonnegative-Flonum (icosahedron-radius->edge-length r radius-type))
    
    (create-geometry-object geo:icosahedron:side
                            #:surface (geo-side-surface border) edge pattern
                            #:extent (geo-shape-plain-extent (icosahedron-edge-length->side-outline-size sidelength))
                            #:id id
                            r radius-type sidelength)))

(define geo-icosahedron-over-projection : (->* (Real)
                                               (3D-Radius-Type #:id (Option Symbol) #:rotation Real #:radian? Boolean
                                                               #:edge Maybe-Stroke-Paint #:border Maybe-Stroke-Paint #:fill Maybe-Fill-Paint)
                                               Geo:Icosahedron:Over)
  (lambda [#:id [id #false] #:edge [edge (void)] #:border [border (void)] #:fill [pattern (void)] #:rotation [rotation 0.0] #:radian? [radian? #true]
           radius [radius-type 'vertex]]
    (define r : Nonnegative-Flonum (~length radius))
    (define R : Nonnegative-Flonum (icosahedron-radius->circumsphere-radius r radius-type))
    
    (create-geometry-object geo:icosahedron:over
                            #:surface (geo-over-surface border) edge pattern
                            #:extent (geo-shape-plain-extent (* 2.0 R))
                            #:id id
                            r radius-type R (~radian rotation radian?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-side-surface : (-> Maybe-Stroke-Paint Geo-Surface-Create)
  (lambda [alt-bdr]
    (λ [self]
      (with-asserts ([self geo:icosahedron:side?])
        (dc_icosahedron_side_proj create-abstract-surface
                                  (geo:icosahedron:side-edge-length self)
                                  (current-stroke-source) (current-fill-source) (geo-select-stroke-paint alt-bdr)
                                  (default-geometry-density))))))

(define geo-over-surface : (-> Maybe-Stroke-Paint Geo-Surface-Create)
  (lambda [alt-bdr]
    (λ [self]
      (with-asserts ([self geo:icosahedron:over?])
        (dc_icosahedron_over_proj create-abstract-surface
                                  (geo:icosahedron:over-circumsphere-radius self) (geo:icosahedron:over-rotation self)
                                  (current-stroke-source) (current-fill-source) (geo-select-stroke-paint alt-bdr)
                                  (default-geometry-density))))))

#lang typed/racket/base

(provide (all-defined-out) 3D-Radius-Type)

(require digimon/metrics)

(require "../../paint.rkt")
(require "../../stroke.rkt")

(require "../source.rkt")
(require "../convert.rkt")

(require "../unsafe/dc/icosahedron.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:icosahedron:side geo
  ([radius : Nonnegative-Flonum]
   [radius-type : 3D-Radius-Type])
  #:type-name Geo:Icosahedron:Side
  #:transparent)

(struct geo:icosahedron:over geo
  ([radius : Nonnegative-Flonum]
   [radius-type : 3D-Radius-Type]
   [rotation : Flonum])
  #:type-name Geo:Icosahedron:Over
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-icosahedron-side-projection : (->* (Real)
                                               (3D-Radius-Type #:id (Option Symbol)
                                                               #:edge (Option Stroke-Paint) #:border (Option Stroke-Paint) #:fill (Option Fill-Paint))
                                               Geo:Icosahedron:Side)
  (lambda [#:id [id #false] #:edge [edge #false] #:border [border #false] #:fill [pattern #false]
           radius [radius-type 'vertex]]
    (create-geometry-object geo:icosahedron:side
                            #:with [(geo-shape-surface-wrapper geo-side-surface (stroke-paint->source* edge) (stroke-paint->source* border) pattern #false)] #:id id
                            (~length radius) radius-type)))

(define geo-icosahedron-over-projection : (->* (Real)
                                               (3D-Radius-Type #:id (Option Symbol) #:rotation Real #:radian? Boolean
                                                               #:edge (Option Stroke-Paint) #:border (Option Stroke-Paint) #:fill (Option Fill-Paint))
                                               Geo:Icosahedron:Over)
  (lambda [#:id [id #false] #:edge [edge #false] #:border [border #false] #:fill [pattern #false] #:rotation [rotation 0.0] #:radian? [radian? #true]
           radius [radius-type 'vertex]]
    (create-geometry-object geo:icosahedron:over
                            #:with [(geo-shape-surface-wrapper geo-over-surface (stroke-paint->source* edge) (stroke-paint->source* border) pattern #false)] #:id id
                            (~length radius) radius-type (~radian rotation radian?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-side-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:icosahedron:side?])
      (dc_icosahedron_side_proj create-abstract-surface
                                (geo:icosahedron:side-radius self) (geo:icosahedron:side-radius-type self)
                                (default-stroke) (fill-paint->source* (default-fill-paint)) (default-border)
                                (default-geometry-density)))))

(define geo-over-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:icosahedron:over?])
      (dc_icosahedron_over_proj create-abstract-surface
                                (geo:icosahedron:over-radius self) (geo:icosahedron:over-radius-type self) (geo:icosahedron:over-rotation self)
                                (default-stroke) (fill-paint->source* (default-fill-paint)) (default-border)
                                (default-geometry-density)))))

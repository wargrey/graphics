#lang typed/racket/base

(provide (all-defined-out))

(require "edge/style.rkt")
(require "edge/type.rkt")
(require "edge/metrics.rkt")

(require "paint.rkt")
(require "../../paint.rkt")
(require "../../stroke.rkt")

(require "../convert.rkt")
(require "../unsafe/dc/edge.rkt")

(require "../geometry/dot.rkt")
(require "../geometry/footprint.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:edge geo
  ([footprints : (Listof Geo-Path-Print)]
   [source : Float-Complex]
   [target : Float-Complex]
   [origin : Float-Complex]
   [bbox-offset : Float-Complex]
   [line-offset : (Option Float-Complex)]
   [source-shape : (Listof Geo-Path-Clean-Print)]
   [target-shape : (Listof Geo-Path-Clean-Print)])
  #:type-name Geo:Edge
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-edge : (->* ((List* Geo-Path-Clean-Print Geo-Path-Clean-Print (Listof Geo-Path-Clean-Print)))
                        (#:id (Option Symbol) #:stroke Maybe-Stroke-Paint
                         #:source-shape (Option Geo-Edge-Shape) #:target-shape (Option Geo-Edge-Shape))
                        Geo:Edge)
  (lambda [footprints #:id [id #false] #:stroke [stroke (void)] #:source-shape [src-shape #false] #:target-shape [tgt-shape #false]]
    (define thickness : Nonnegative-Flonum (let ([s (geo-edge-select-line-paint stroke)]) (if (stroke? s) (stroke-width s) 0.0)))
    (define line-offset : Nonnegative-Flonum (* thickness 0.5))
    (define-values (spt srad ept erad) (geo-path-end-points footprints))
    (define-values (e.x e.y e.w e.h) (geo-path-ink-box footprints))

    ;; NOTE: we move the end shapes to their absolute position, and no need to translate them when drawing
    (define-values (src-prints s.x s.y s.w s.h) (geo-edge-shape-metrics src-shape thickness srad spt))
    (define-values (tgt-prints t.x t.y t.w t.h) (geo-edge-shape-metrics tgt-shape thickness erad ept))

    ;; NOTE: for shapes having outline stroke, simply add the thickness here
    (define ssoffset : Nonnegative-Flonum (* (stroke-width default-shape-stroke) 0.5))
    (define-values (lx ty) (values (min e.x (- s.x ssoffset) (- t.x ssoffset)) (min e.y (- s.y ssoffset) (- t.y ssoffset))))
    (define-values (rx by) (values (max (+ e.x e.w) (+ s.x s.w ssoffset) (+ t.x t.w ssoffset)) (max (+ e.y e.h) (+ s.y s.h ssoffset) (+ t.y t.h ssoffset))))
    (define-values (xoff yoff width height x-stroke? y-stroke?) (point2d->window +nan.0+nan.0i lx ty rx by))
    
    (create-geometry-object geo:edge
                            #:surface (geo-edge-surface width height x-stroke? y-stroke?) stroke
                            #:extent (geo-stroke-extent-wrapper (geo-shape-plain-extent width height 0.0 0.0) stroke x-stroke? y-stroke?)
                            #:id id
                            footprints spt ept (make-rectangular lx ty)
                            (make-rectangular xoff yoff) (make-rectangular line-offset line-offset)
                            src-prints tgt-prints)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-edge-pin-at-position : (->* (Geo:Edge) ((Option Float-Complex)) Float-Complex)
  (lambda [self [maybe-pt #false]]
    (define S : Float-Complex (geo:edge-source self))
    (define O : Float-Complex (geo:edge-origin self))
    (define ->S : Float-Complex (- S O))
    
    (- (or maybe-pt S)
       (make-rectangular (abs (real-part ->S))
                         (abs (imag-part ->S)))
       (or (geo:edge-line-offset self)
           0.0+0.0i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-edge-surface : (-> Nonnegative-Flonum Nonnegative-Flonum Boolean Boolean Geo-Surface-Create)
  (lambda [width height x-stroke? y-stroke?]
    (λ [self]
      (with-asserts ([self geo:edge?])
        (define offset (geo:edge-bbox-offset self))
        (define stroke (current-stroke-source))
        (define color (and stroke (stroke-color stroke)))
        (define shape-stroke (desc-stroke default-shape-stroke #:color color))
        (dc_edge create-abstract-surface
                 width height (geo:edge-footprints self) (real-part offset) (imag-part offset) x-stroke? y-stroke? stroke
                 (vector-immutable (geo:edge-source-shape self) shape-stroke color)
                 (vector-immutable (geo:edge-target-shape self) shape-stroke color)
                 (default-geometry-density))))))
  
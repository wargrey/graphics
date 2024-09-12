#lang typed/racket/base

(provide (all-defined-out))

(require "style.rkt")
(require "type.rkt")
(require "metrics.rkt")

(require "../unsafe/edge.rkt")

(require geofun/paint)
(require geofun/stroke)

(require geofun/digitama/dc/paint)
(require geofun/digitama/convert)
(require geofun/digitama/geometry/dot)
(require geofun/digitama/geometry/footprint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct dia:edge geo
  ([footprints : (Listof Geo-Path-Print)]
   [source : Float-Complex]
   [target : Float-Complex]
   [origin : Float-Complex]
   [bbox-offset : Float-Complex]
   [line-offset : (Option Float-Complex)]
   [source-shape : (Listof Geo-Path-Clean-Print)]
   [target-shape : (Listof Geo-Path-Clean-Print)])
  #:type-name Dia:Edge
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-edge : (->* ((List* Geo-Path-Clean-Print Geo-Path-Clean-Print (Listof Geo-Path-Clean-Print)))
                        (#:id (Option Symbol) #:stroke Maybe-Stroke-Paint
                         #:source-shape (Option Dia-Edge-Shape) #:target-shape (Option Dia-Edge-Shape))
                        Dia:Edge)
  (lambda [footprints #:id [id #false] #:stroke [stroke (void)] #:source-shape [src-shape #false] #:target-shape [tgt-shape #false]]
    (define thickness : Nonnegative-Flonum (let ([s (dia-edge-select-line-paint stroke)]) (if (stroke? s) (stroke-width s) 0.0)))
    (define line-offset : Nonnegative-Flonum (* thickness 0.5))
    (define-values (spt srad ept erad) (geo-path-end-points footprints))
    (define-values (e.x e.y e.w e.h) (geo-path-ink-box footprints))

    ;; NOTE: we move the end shapes to their absolute position, and no need to translate them when drawing
    (define-values (src-prints s.x s.y s.w s.h) (dia-edge-shape-metrics src-shape thickness srad spt))
    (define-values (tgt-prints t.x t.y t.w t.h) (dia-edge-shape-metrics tgt-shape thickness erad ept))

    ;; NOTE: for shapes having outline stroke, simply add the thickness here
    (define ssoffset : Nonnegative-Flonum (* (stroke-width default-dia-shape-stroke) 0.5))
    (define-values (lx ty) (values (min e.x (- s.x ssoffset) (- t.x ssoffset)) (min e.y (- s.y ssoffset) (- t.y ssoffset))))
    (define-values (rx by) (values (max (+ e.x e.w) (+ s.x s.w ssoffset) (+ t.x t.w ssoffset)) (max (+ e.y e.h) (+ s.y s.h ssoffset) (+ t.y t.h ssoffset))))
    (define-values (xoff yoff width height x-stroke? y-stroke?) (point2d->window +nan.0+nan.0i lx ty rx by))
    
    (create-geometry-object dia:edge
                            #:surface (dia-edge-surface width height x-stroke? y-stroke?) stroke
                            #:extent (geo-stroke-extent-wrapper (geo-shape-plain-extent width height 0.0 0.0) stroke x-stroke? y-stroke?)
                            #:id id
                            footprints spt ept (make-rectangular lx ty)
                            (make-rectangular xoff yoff) (make-rectangular line-offset line-offset)
                            src-prints tgt-prints)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-edge-pin-at-position : (->* (Dia:Edge) ((Option Float-Complex)) Float-Complex)
  (lambda [self [maybe-pt #false]]
    (define S : Float-Complex (dia:edge-source self))
    (define O : Float-Complex (dia:edge-origin self))
    (define ->S : Float-Complex (- S O))
    
    (- (or maybe-pt S)
       (make-rectangular (abs (real-part ->S))
                         (abs (imag-part ->S)))
       (or (dia:edge-line-offset self)
           0.0+0.0i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-edge-surface : (-> Nonnegative-Flonum Nonnegative-Flonum Boolean Boolean Geo-Surface-Create)
  (lambda [width height x-stroke? y-stroke?]
    (λ [self]
      (with-asserts ([self dia:edge?])
        (define offset (dia:edge-bbox-offset self))
        (define stroke (current-stroke-source))
        (define color (and stroke (stroke-color stroke)))
        (define shape-stroke (desc-stroke default-dia-shape-stroke #:color color))
        (dc_edge create-abstract-surface
                 width height (dia:edge-footprints self) (real-part offset) (imag-part offset) x-stroke? y-stroke? stroke
                 (vector-immutable (dia:edge-source-shape self) shape-stroke color)
                 (vector-immutable (dia:edge-target-shape self) shape-stroke color)
                 (default-geometry-density))))))
  
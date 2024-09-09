#lang typed/racket/base

(provide (all-defined-out))

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
   [draw-offset : Float-Complex]
   [thickness-offset : (Option Float-Complex)]
   [wing-angle : (Option Flonum)])
  #:type-name Geo:Edge
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-edge : (->* ((Listof Geo-Path-Print)) (#:id (Option Symbol) #:stroke Maybe-Stroke-Paint) Geo:Edge)
  (lambda [footprints #:id [id #false] #:stroke [stroke (void)]]
    (define-values (lx ty w h source target) (geo-path-ink-box* footprints))
    (define-values (xoff yoff width height x-stroke? y-stroke?) (point2d->window +nan.0+nan.0i lx ty (+ lx w) (+ ty h)))
    (define thickness-offset (if (stroke? stroke) (* (stroke-width stroke) 0.5) 0.0))

    (create-geometry-object geo:edge
                            #:surface (geo-edge-surface width height x-stroke? y-stroke?) stroke
                            #:extent (geo-stroke-extent-wrapper (geo-shape-plain-extent width height 0.0 0.0) stroke x-stroke? y-stroke?)
                            #:id id
                            footprints source target (make-rectangular lx ty)
                            (make-rectangular xoff yoff) (make-rectangular thickness-offset thickness-offset)
                            #false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-edge-pin-at-position : (->* (Geo:Edge) ((Option Float-Complex)) Float-Complex)
  (lambda [self [maybe-pt #false]]
    (define S : Float-Complex (geo:edge-source self))
    (define O : Float-Complex (geo:edge-origin self))
    (define ->S : Float-Complex (- S O))
    
    (- (or maybe-pt S)
       (make-rectangular (abs (real-part ->S))
                         (abs (imag-part ->S)))
       (or (geo:edge-thickness-offset self)
           0.0+0.0i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-edge-surface : (-> Nonnegative-Flonum Nonnegative-Flonum Boolean Boolean Geo-Surface-Create)
  (lambda [width height x-stroke? y-stroke?]
    (λ [self]
      (with-asserts ([self geo:edge?])
        (define offset (geo:edge-draw-offset self))
        (dc_edge create-abstract-surface
                 width height (geo:edge-footprints self) (real-part offset) (imag-part offset) x-stroke? y-stroke?
                 (current-stroke-source) (default-geometry-density))))))
  
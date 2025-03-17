#lang typed/racket/base

(provide (all-defined-out))

(require "polygon.rkt")

(require "../convert.rkt")
(require "../geometry/dot.rkt")
(require "../geometry/sample.rkt")

(require "../../paint.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-geo-function-samples : (Parameterof Positive-Index)(make-parameter 512))

(define default-geo-coordinate-transform : (-> Flonum Flonum Float-Complex)
  (lambda [cg-x cg-y]
    (make-rectangular cg-x (- cg-y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-cartesian
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:id [id : (Option Symbol) #false] #:translate [translate : Point2D 0.0] #:window [window : (Option Point2D) #false]
           #:samples [samples : Positive-Index (default-geo-function-samples)]
           #:transform [transform : (-> Flonum Flonum Float-Complex) default-geo-coordinate-transform]
           [f : (-> Flonum (Option Flonum))] [scale : Point2D] [xmin : Real] [xmax : Real]
           [maybe-ymin : (Option Real) #false] [maybe-ymax : (Option Real) #false]] : Geo:Polyline
    (define xs : (Listof Real) (geo-linear-samples xmin xmax samples))
    (define-values (dx dy) (point2d-values translate))
    (define-values (sx sy) (point2d-scale-values scale))
    (define ymin (real->double-flonum (or maybe-ymin -inf.0)))
    (define ymax (real->double-flonum (or maybe-ymax +inf.0)))
    (define-values (prints lx ty rx by) (~cartesian2ds f xs ymin ymax dx dy sx sy transform))
    (define-values (xoff yoff width height x-stroke? y-stroke?) (point2d->window (or window +nan.0+nan.0i) lx ty rx by))
    
    (create-geometry-object geo:polyline
                            #:with [id (geo-draw-polyline stroke)
                                       (geo-shape-extent width height 0.0 0.0)
                                       (geo-shape-outline stroke x-stroke? y-stroke?)]
                            prints xoff yoff #false)))


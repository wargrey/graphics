#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require "../paint.rkt")
(require "../../paint.rkt")

(require "../convert.rkt")
(require "../unsafe/dc/path.rkt")
(require "../unsafe/dc/shape.rkt")

(require "../geometry/dot.rkt")
(require "../geometry/radius.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:regular-polygon geo
  ([n : Index]
   [k : Positive-Index]
   [radius : Nonnegative-Flonum]
   [raidus-type : 2D-Radius-Type]
   [rotation : Flonum])
  #:type-name Geo:Regular-Polygon
  #:transparent)

(struct geo:polygon geo
  ([prints : (Listof Float-Complex)]
   [tx : Flonum]
   [ty : Flonum])
  #:type-name Geo:Polygon
  #:transparent)

(struct geo:polyline geo
  ([prints : (Listof Float-Complex)]
   [tx : Flonum]
   [ty : Flonum]
   [closed? : Boolean])
  #:type-name Geo:Polyline
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-regular-polygon
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           #:id [id : (Option Symbol) #false]
           #:inscribed? [inscribed? : Boolean #false]
           [n : Integer] [radius : Real] [rotation : Real 0.0]] : Geo:Regular-Polygon
    (define R : Nonnegative-Flonum (~length radius))
    (define N : Index (if (index? n) n 0))
    (define rtype : 2D-Radius-Type (if inscribed? 'edge 'vertex))
    (define d : Nonnegative-Flonum
      (* 2.0 (if (> N 0)
                 (regular-polygon-radius->circumsphere-radius N R rtype)
                 R)))
    
    (create-geometry-object geo:regular-polygon
                            #:with [id (geo-draw-regular-polygon stroke pattern)
                                       (geo-shape-extent d d 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            N 1 R rtype (real->double-flonum rotation))))

(define geo-star-polygon
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           #:id [id : (Option Symbol) #false]
           #:inscribed? [inscribed? : Boolean #false]
           [n : Integer] [step : Integer] [radius : Real] [rotation : Real 0.0]] : Geo:Regular-Polygon
    (define R : Nonnegative-Flonum (~length radius))
    (define N : Index (if (index? n) n 0))
    (define K : Positive-Index (if (and (index? step) (> step 0)) step 1))
    (define rtype : 2D-Radius-Type (if inscribed? 'edge 'vertex))
    (define d : Nonnegative-Flonum
      (* 2.0 (if (> N 0)
                 (regular-polygon-radius->circumsphere-radius N R rtype)
                 R)))
    
    (create-geometry-object geo:regular-polygon
                            #:with [id (geo-draw-regular-polygon stroke pattern)
                                       (geo-shape-extent d d 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            N K R rtype (real->double-flonum rotation))))

(define geo-polygon
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           #:id [id : (Option Symbol) #false]
           #:scale [scale : Point2D 1.0]
           #:offset [offset : Complex 0.0+0.0i]
           #:window [window : (Option Point2D) #false]
           [pts : (U Point2D (Listof Point2D))]] : Geo:Polygon
    (define-values (prints lx ty rx by) (~point2ds (if (list? pts) pts (list pts)) offset scale))
    (define-values (xoff yoff width height x-stroke? y-stroke?) (point2d->window (or window +nan.0+nan.0i) lx ty rx by))
    
    (create-geometry-object geo:polygon
                            #:with [id (geo-draw-polygon stroke pattern)
                                       (geo-shape-extent width height 0.0 0.0)
                                       (geo-shape-outline stroke x-stroke? y-stroke?)]
                            prints xoff yoff)))

(define geo-polyline
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:id [id : (Option Symbol) #false]
           #:scale [scale : Point2D 1.0]
           #:offset [offset : Complex 0.0+0.0i]
           #:window [window : (Option Point2D) #false]
           #:close? [close? : Boolean #false]
           [pts : (U Point2D (Listof Point2D))]] : Geo:Polyline
    (define-values (prints lx ty rx by) (~point2ds (if (list? pts) pts (list pts)) offset scale))
    (define-values (xoff yoff width height x-stroke? y-stroke?) (point2d->window (or window +nan.0+nan.0i) lx ty rx by))
    
    (create-geometry-object geo:polyline
                            #:with [id (geo-draw-polyline stroke)
                                       (geo-shape-extent width height 0.0 0.0)
                                       (geo-shape-outline stroke x-stroke? y-stroke?)]
                            prints xoff yoff close?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-draw-regular-polygon : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill]
    (λ [self cr x0 y0 width height]
      (when (geo:regular-polygon? self)
        (define n (geo:regular-polygon-n self))
        
        (if (> n 0)
            (dc_regular_polygon cr x0 y0 width height n 
                                (geo:regular-polygon-k self) (geo:regular-polygon-rotation self)
                                (geo-select-stroke-paint alt-stroke) (geo-select-fill-source alt-fill))
            (dc_ellipse cr x0 y0 width height
                        (geo-select-stroke-paint alt-stroke) (geo-select-fill-source alt-fill)
                        null))))))

(define geo-draw-polygon : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill]
    (λ [self cr x0 y0 width height]
      (when (geo:polygon? self)
        (dc_polygon cr (+ x0 (geo:polygon-tx self)) (+ y0 (geo:polygon-ty self)) width height
                    (geo:polygon-prints self)
                    (geo-select-stroke-paint alt-stroke)
                    (geo-select-fill-source alt-fill))))))

(define geo-draw-polyline : (-> Maybe-Stroke-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke]
    (λ [self cr x0 y0 width height]
      (when (geo:polyline? self)
        (dc_polyline cr (+ x0 (geo:polyline-tx self)) (+ y0 (geo:polyline-ty self)) width height
                     (geo:polyline-prints self)
                     (geo-select-stroke-paint* alt-stroke)
                     (geo:polyline-closed? self))))))

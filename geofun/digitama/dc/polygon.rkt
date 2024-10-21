#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require "paint.rkt")
(require "../../paint.rkt")

(require "../convert.rkt")
(require "../unsafe/path.rkt")
(require "../unsafe/dc/shape.rkt")

(require "../geometry/dot.rkt")
(require "../geometry/radius.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:regular-polygon geo
  ([n : Index]
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
(define geo-regular-polygon : (->* (Integer Real)
                                   (Real #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint #:radian? Boolean #:inscribed? Boolean #:id (Option Symbol))
                                   Geo:Regular-Polygon)
  (lambda [n radius [rotation 0.0] #:id [id #false] #:stroke [stroke (void)] #:fill [pattern (void)] #:radian? [radian? #true] #:inscribed? [inscribed? #false]]
    (define R : Nonnegative-Flonum (~length radius))
    (define N : Index (if (index? n) n 0))
    (define rtype : 2D-Radius-Type (if inscribed? 'edge 'vertex))
    (define d : Nonnegative-Flonum
      (* 2.0 (if (> N 0)
                 (regular-polygon-radius->circumsphere-radius N R rtype)
                 R)))
    
    (create-geometry-object geo:regular-polygon (geo-regular-polygon-surface stroke pattern)
                            #:extent (geo-shape-plain-extent d d 0.0 0.0)
                            #:id id
                            N R rtype (~radian rotation radian?))))

(define geo-polygon : (->* ((U Point2D (Listof Point2D)))
                           (Real Real
                                 #:id (Option Symbol) #:scale Point2D #:window (Option Point2D)
                                 #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint #:fill-rule (Option Symbol))
                           Geo:Polygon)
  (lambda [#:id [id #false] #:scale [scale 1.0] #:window [window #false]
           #:stroke [stroke (void)] #:fill [pattern (void)] #:fill-rule [rule #false]
           pts [dx 0.0] [dy 0.0]]
    (define-values (prints lx ty rx by) (~point2ds (if (list? pts) pts (list pts)) dx dy scale))
    (define-values (xoff yoff width height x-stroke? y-stroke?) (point2d->window (or window +nan.0+nan.0i) lx ty rx by))
    
    (create-geometry-object geo:polygon (geo-polygon-surface stroke pattern rule)
                            #:extent (geo-shape-plain-extent width height 0.0 0.0)
                            #:id id
                            prints xoff yoff)))

(define geo-polyline : (->* ((U Point2D (Listof Point2D)))
                            (Real Real #:id (Option Symbol) #:scale Point2D #:window (Option Point2D) #:stroke Maybe-Stroke-Paint #:close? Boolean)
                            Geo:Polyline)
  (lambda [#:id [id #false] #:scale [scale 1.0] #:window [window #false] #:stroke [stroke (void)] #:close? [close? #false]
           pts [dx 0.0] [dy 0.0]]
    (define-values (prints lx ty rx by) (~point2ds (if (list? pts) pts (list pts)) dx dy scale))
    (define-values (xoff yoff width height x-stroke? y-stroke?) (point2d->window (or window +nan.0+nan.0i) lx ty rx by))
    
    (create-geometry-object geo:polyline (geo-polyline-surface stroke)
                            #:extent (geo-shape-plain-extent width height 0.0 0.0)
                            #:id id
                            prints xoff yoff close?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-regular-polygon-surface : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill]
    (λ [self cr x0 y0 width height]
      (when (geo:regular-polygon? self)
        (define n (geo:regular-polygon-n self))
        
        (if (> n 0)
            (dc_regular_polygon cr x0 y0 width height n 
                                (geo:regular-polygon-rotation self)
                                (geo-select-stroke-paint alt-stroke) (geo-select-fill-source alt-fill))
            (dc_ellipse cr x0 y0 width height
                        (geo-select-stroke-paint alt-stroke) (geo-select-fill-source alt-fill)
                        null))))))

(define geo-polygon-surface : (-> Maybe-Stroke-Paint Maybe-Fill-Paint (Option Symbol) Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill alt-rule]
    (λ [self cr x0 y0 width height]
      (when (geo:polygon? self)
        (dc_polygon cr x0 y0 width height (geo:polygon-prints self)
                    (geo-select-stroke-paint alt-stroke) (geo-select-fill-source alt-fill)
                    (or alt-rule (default-fill-rule)))))))

(define geo-polyline-surface : (-> Maybe-Stroke-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke]
    (λ [self cr x0 y0 width height]
      (when (geo:polyline? self)
        (dc_polyline cr x0 y0 width height (geo:polyline-prints self)
                     ;(geo:polyline-tx self) (geo:polyline-ty self) xstroke? ystroke?
                     (geo-select-stroke-paint* alt-stroke) (geo:polyline-closed? self))))))

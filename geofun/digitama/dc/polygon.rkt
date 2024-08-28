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
  ([prints : (Listof (Pairof Char Float-Complex))]
   [xoff : Flonum]
   [yoff : Flonum])
  #:type-name Geo:Polygon
  #:transparent)

(struct geo:polyline geo
  ([prints : (Listof (Pairof Char Float-Complex))]
   [xoff : Flonum]
   [yoff : Flonum]
   [closed? : Boolean])
  #:type-name Geo:Polyline
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-regular-polygon : (->* (Integer Real)
                                   (Real #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint #:radian? Boolean #:inscribed? Boolean #:id (Option Symbol))
                                   Geo:Regular-Polygon)
  (lambda [n radius [rotation 0.0] #:id [id #false] #:stroke [stroke (void)] #:fill [pattern (void)] #:radian? [radian? #true] #:inscribed? [inscribed? #false]]
    (define R : Nonnegative-Flonum (~length radius))
    (define rtype : 2D-Radius-Type (if inscribed? 'edge 'vertex))
    
    (create-geometry-object geo:regular-polygon
                            #:surface geo-regular-polygon-surface stroke pattern
                            #:extent geo-regular-polygon-extent
                            #:id id
                            (if (index? n) n 0) R rtype (~radian rotation radian?))))

(define geo-polygon : (->* ((U Point2D (Listof Point2D)))
                           (Real Real
                                 #:id (Option Symbol) #:scale Point2D #:window Point2D
                                 #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint #:fill-rule (Option Symbol))
                           Geo:Polygon)
  (lambda [#:id [id #false] #:scale [scale 1.0] #:window [window 0.0-0.0i]
           #:stroke [stroke (void)] #:fill [pattern (void)] #:fill-rule [rule #false]
           pts [dx 0.0] [dy 0.0]]
    (define-values (prints lx ty rx by) (~point2ds (if (list? pts) pts (list pts)) dx dy scale))
    (define-values (xoff yoff width height) (point2d->window window lx ty rx by))
    
    (create-geometry-object geo:polygon
                            #:surface (geo-polygon-surface width height) stroke pattern rule
                            #:extent (geo-stroke-extent-wrapper (geo-shape-plain-extent width height 0.0 0.0) stroke)
                            #:id id
                            prints xoff yoff)))

(define geo-polyline : (->* ((U Point2D (Listof Point2D)))
                            (Real Real #:id (Option Symbol) #:scale Point2D #:window Point2D #:stroke Maybe-Stroke-Paint #:close? Boolean)
                            Geo:Polyline)
  (lambda [#:id [id #false] #:scale [scale 1.0] #:window [window 0.0-0.0i] #:stroke [stroke (void)] #:close? [close? #false]
           pts [dx 0.0] [dy 0.0]]
    (define-values (prints lx ty rx by) (~point2ds (if (list? pts) pts (list pts)) dx dy scale))
    (define-values (xoff yoff width height) (point2d->window window lx ty rx by))
    
    (create-geometry-object geo:polyline
                            #:surface (geo-polyline-surface width height) stroke
                            #:extent (geo-stroke-extent-wrapper (geo-shape-plain-extent width height 0.0 0.0) stroke)
                            #:id id
                            prints xoff yoff close?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-regular-polygon-extent : Geo-Calculate-Extent
  (lambda [self]
    (with-asserts ([self geo:regular-polygon?])
      (define n (geo:regular-polygon-n self))
      (define R (geo:regular-polygon-radius self))

      (define d : Nonnegative-Flonum
        (* 2.0
           (if (> n 0)
               (regular-polygon-radius->circumsphere-radius n R (geo:regular-polygon-raidus-type self))
               R)))
      
      (values d d #false))))

(define geo-regular-polygon-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:regular-polygon?])
      (define n (geo:regular-polygon-n self))
      (define R (geo:regular-polygon-radius self))

      (if (> n 0)
          (dc_regular_polygon create-abstract-surface
                              n (regular-polygon-radius->circumsphere-radius n R (geo:regular-polygon-raidus-type self))
                              (geo:regular-polygon-rotation self)
                              (current-stroke-source) (current-fill-source)
                              (default-geometry-density))
          (dc_circle create-abstract-surface
                     R (current-stroke-source) (current-fill-source)
                     (default-geometry-density))))))

(define geo-polygon-surface : (-> Nonnegative-Flonum Nonnegative-Flonum Geo-Surface-Create)
  (lambda [width height]
    (λ [self]
      (with-asserts ([self geo:polygon?])
        (dc_polygon create-abstract-surface
                    width height (geo:polygon-prints self) (geo:polygon-xoff self) (geo:polygon-yoff self)
                    (current-stroke-source) (current-fill-source) (default-fill-rule)
                    (default-geometry-density))))))

(define geo-polyline-surface : (-> Nonnegative-Flonum Nonnegative-Flonum Geo-Surface-Create)
  (lambda [width height]
    (λ [self]
      (with-asserts ([self geo:polyline?])
        (dc_polyline create-abstract-surface
                     width height (geo:polyline-prints self) (geo:polyline-xoff self) (geo:polyline-yoff self)
                     (current-stroke-source*) (geo:polyline-closed? self)
                     (default-geometry-density))))))
  
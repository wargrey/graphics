#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require "paint.rkt")
(require "../../paint.rkt")

(require "../convert.rkt")
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
  ([xs : (Listof Flonum)]
   [ys : (Listof Flonum)]
   [xoff : Flonum]
   [yoff : Flonum])
  #:type-name Geo:Polygon
  #:transparent)

(struct geo:polyline geo
  ([xs : (Listof Flonum)]
   [ys : (Listof Flonum)]
   [xoff : Flonum]
   [yoff : Flonum]
   [closed? : Boolean])
  #:type-name Geo:Polyline
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-regular-polygon : (->* (Integer Real)
                                   (Real #:border Maybe-Stroke-Paint #:fill Maybe-Fill-Paint #:radian? Boolean #:inscribed? Boolean #:id (Option Symbol))
                                   Geo:Regular-Polygon)
  (lambda [n radius [rotation 0.0] #:id [id #false] #:border [border (void)] #:fill [pattern (void)] #:radian? [radian? #true] #:inscribed? [inscribed? #false]]
    (define R : Nonnegative-Flonum (~length radius))
    (define rtype : 2D-Radius-Type (if inscribed? 'edge 'vertex))
    
    (create-geometry-object geo:regular-polygon
                            #:with [(geo-shape-surface-wrapper geo-regular-polygon-surface border pattern) geo-regular-polygon-bbox] #:id id
                            (if (index? n) n 0) R rtype (~radian rotation radian?))))

(define geo-polygon : (->* ((U Point2D (Listof Point2D)))
                           (Real Real
                                 #:id (Option Symbol) #:scale Point2D #:window Point2D
                                 #:border Maybe-Stroke-Paint #:fill Maybe-Fill-Paint #:fill-rule (Option Symbol))
                           Geo:Polygon)
  (lambda [#:id [id #false] #:scale [scale 1.0] #:window [window 0]
           #:border [border (void)] #:fill [pattern (void)] #:fill-rule [rule #false]
           pts [dx 0.0] [dy 0.0]]
    (define-values (xs ys lx ty rx by) (~point2ds (if (list? pts) pts (list pts)) dx dy scale))
    (define-values (xoff yoff width height) (point2d->window window lx ty rx by))
    (define polygon-bbox : Geo-Calculate-BBox (geo-shape-plain-bbox width height))
    
    (create-geometry-object geo:polygon
                            #:with [(geo-shape-surface-wrapper geo-polygon-surface border pattern rule) polygon-bbox] #:id id
                            xs ys xoff yoff)))

(define geo-polyline : (->* ((U Point2D (Listof Point2D)))
                            (Real Real #:id (Option Symbol) #:scale Point2D #:window Point2D #:stroke Maybe-Stroke-Paint #:close? Boolean)
                            Geo:Polyline)
  (lambda [#:id [id #false] #:scale [scale 1.0] #:window [window 0] #:stroke [stroke (void)] #:close? [close? #false]
           pts [dx 0.0] [dy 0.0]]
    (define-values (xs ys lx ty rx by) (~point2ds (if (list? pts) pts (list pts)) dx dy scale))
    (define-values (xoff yoff width height) (point2d->window window lx ty rx by))
    (define polyline-bbox : Geo-Calculate-BBox (geo-shape-plain-bbox width height))
    
    (create-geometry-object geo:polyline
                            #:with [(geo-shape-surface-wrapper geo-polygon-surface stroke) polyline-bbox] #:id id
                            xs ys xoff yoff close?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-regular-polygon-bbox : Geo-Calculate-BBox
  (lambda [self]
    (with-asserts ([self geo:regular-polygon?])
      (define n (geo:regular-polygon-n self))
      (define R (geo:regular-polygon-radius self))

      (define d : Nonnegative-Flonum
        (* (if (> n 0)
               (regular-polygon-radius->circumsphere-radius n R (geo:regular-polygon-raidus-type self))
               R)
           2.0))
      (values 0.0 0.0 d d))))

(define geo-regular-polygon-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:regular-polygon?])
      (define n (geo:regular-polygon-n self))
      (define R (geo:regular-polygon-radius self))

      (if (> n 0)
          (dc_regular_polygon create-abstract-surface
                              n (regular-polygon-radius->circumsphere-radius n R (geo:regular-polygon-raidus-type self))
                              (geo:regular-polygon-rotation self)
                              (current-border-source) (current-fill-source)
                              (default-geometry-density))
          (dc_circle create-abstract-surface
                     R (current-border-source) (current-fill-source)
                     (default-geometry-density))))))

(define geo-polygon-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:polygon?])
      (define-values (width height) (geo-flsize self))
      (dc_polygon create-abstract-surface
                  width height (geo:polygon-xs self) (geo:polygon-ys self)
                  (geo:polygon-xoff self) (geo:polygon-yoff self)
                  (current-border-source) (current-fill-source) (default-fill-rule)
                  (default-geometry-density)))))

(define geo-polyline-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:polyline?])
      (define-values (width height) (geo-flsize self))
      
      (dc_polyline create-abstract-surface
                   width height (geo:polyline-xs self) (geo:polyline-ys self)
                   (geo:polyline-xoff self) (geo:polyline-yoff self)
                   (current-stroke-source*) (geo:polyline-closed? self)
                   (default-geometry-density)))))

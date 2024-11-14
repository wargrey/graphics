#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require "../paint.rkt")
(require "../../paint.rkt")

(require "../convert.rkt")
(require "../unsafe/dc/shape.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:circle geo
  ([radius : Nonnegative-Flonum]
   [diameters : (Listof Flonum)])
  #:type-name Geo:Circle
  #:transparent)

(struct geo:ellipse geo
  ([a : Nonnegative-Flonum]
   [b : Nonnegative-Flonum]
   [diameters : (Listof Flonum)])
  #:type-name Geo:Ellipse
  #:transparent)

(struct geo:sector geo
  ([aradius : Nonnegative-Flonum]
   [bradius : Nonnegative-Flonum]
   [start : Flonum]
   [end : Flonum])
  #:type-name Geo:Sector
  #:transparent)

(struct geo:arc geo
  ([aradius : Nonnegative-Flonum]
   [bradius : Nonnegative-Flonum]
   [start : Flonum]
   [end : Flonum])
  #:type-name Geo:Arc
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-circle
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (void)] #:fill [pattern : Maybe-Fill-Paint (void)]
           #:id [id : (Option Symbol) #false] #:diameters [diameters : (Listof Real) null] #:radian? [radian? : Boolean #true]
           [radius : Real]] : Geo:Circle
    (define r : Nonnegative-Flonum (~length radius))
    
    (create-geometry-object geo:circle
                            #:with [id (geo-draw-ellipse stroke pattern)
                                       (geo-shape-extent (* 2.0 r) 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            r (for/list : (Listof Flonum) ([d (in-list diameters)])
                                (~radian d radian?)))))

(define geo-ellipse
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (void)] #:fill [pattern : Maybe-Fill-Paint (void)]
           #:id [id : (Option Symbol) #false] #:diameters [diameters : (Listof Real) null] #:radian? [radian? : Boolean #true]
           [width : Real] [height : Real -0.618]] : (U Geo:Circle Geo:Ellipse)
    (define-values (w h) (~extent width height))
    (define ellipse-extent : Geo-Calculate-Extent (geo-shape-extent w h 0.0 0.0))
    (define rads : (Listof Flonum) (for/list ([d (in-list diameters)]) (~radian d radian?)))
    
    (if (= w h)
        (create-geometry-object geo:circle
                                #:with [id (geo-draw-ellipse stroke pattern) ellipse-extent (geo-shape-outline stroke)]
                                (* w 0.5) rads)
        (create-geometry-object geo:ellipse
                                #:with [id (geo-draw-ellipse stroke pattern) ellipse-extent (geo-shape-outline stroke)]
                                (* w 0.5) (* h 0.5) rads))))

(define geo-sector
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (void)] #:fill [pattern : Maybe-Fill-Paint (void)]
           #:id [id : (Option Symbol) #false] #:ratio [ratio : Real 1.0] #:radian? [radian? : Boolean #true]
           [radius : Real] [start : Real] [end : Real]] : Geo:Sector
    (define ar : Nonnegative-Flonum (~length radius))
    (define br : Nonnegative-Flonum (if (> ratio 0.0) (abs (/ ar (real->double-flonum ratio))) ar))
    
    (create-geometry-object geo:sector
                            #:with [id (geo-draw-sector stroke pattern)
                                       (geo-shape-extent (* 2.0 ar) (* 2.0 br))
                                       (geo-shape-outline stroke)]
                            ar br (~radian start radian?) (~radian end radian?))))

(define geo-arc
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (void)] #:radian? [radian? : Boolean #true]
           #:id [id : (Option Symbol) #false] #:ratio [ratio : Real 1.0]
           [radius : Real] [start : Real] [end : Real]] : Geo:Arc
    (define ar : Nonnegative-Flonum (~length radius))
    (define br : Nonnegative-Flonum (if (> ratio 0.0) (abs (/ ar (real->double-flonum ratio))) ar))
    
    (create-geometry-object geo:arc
                            #:with [id (geo-draw-arc stroke)
                                       (geo-shape-extent (* 2.0 ar) (* 2.0 br))
                                       (geo-shape-outline stroke)]
                            ar br (~radian start radian?) (~radian end radian?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-draw-ellipse : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill]
    (λ [self cr x0 y0 width height]
      (cond [(geo:ellipse? self)
             (dc_ellipse cr x0 y0 width height
                         (geo-select-stroke-paint alt-stroke) (geo-select-fill-source alt-fill)
                         (geo:ellipse-diameters self))]
            [(geo:circle? self)
             (dc_ellipse cr x0 y0 width height
                         (geo-select-stroke-paint alt-stroke) (geo-select-fill-source alt-fill)
                         (geo:circle-diameters self))]))))

(define geo-draw-arc : (-> Maybe-Stroke-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke]
    (λ [self cr x0 y0 width height]
      (when (geo:arc? self)
        (define-values (ar br) (values (geo:arc-aradius self) (geo:arc-bradius self)))
        (define-values (srad erad) (values (geo:arc-start self) (geo:arc-end self)))
        
        (dc_arc cr x0 y0 width height
                (geo:arc-start self) (geo:arc-end self)
                (geo-select-stroke-paint* alt-stroke) #false #false)))))

(define geo-draw-sector : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill]
    (λ [self cr x0 y0 width height]
      (when (geo:sector? self)
        (define-values (ar br) (values (geo:sector-aradius self) (geo:sector-bradius self)))
        (define-values (srad erad) (values (geo:sector-start self) (geo:sector-end self)))
        
        (dc_arc cr x0 y0 width height srad erad
                (geo-select-stroke-paint alt-stroke)
                (geo-select-fill-source alt-fill) #true)))))

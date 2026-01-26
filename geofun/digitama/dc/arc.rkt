#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

(require "../self.rkt")
(require "../convert.rkt")

(require "../paint.rkt")
(require "../unsafe/dc/shape.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:circle geo
  ([radius : Nonnegative-Flonum]
   [diameters : (Listof Flonum)])
  #:type-name Geo:Circle
  #:transparent)

(struct geo:ring geo
  ([oradius : Nonnegative-Flonum]
   [iradius : Nonnegative-Flonum])
  #:type-name Geo:Ring
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

(struct geo:bullseye geo
  ([radius : Nonnegative-Flonum]
   [rings% : (Listof Nonnegative-Flonum)])
  #:type-name Geo:Bullseye
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-circle
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           #:id [id : (Option Symbol) #false]
           #:diameters [diameters : (Listof Real) null]
           [radius : Real-Length]] : Geo:Circle
    (define r : Nonnegative-Flonum (~dimension radius))
    
    (create-geometry-object geo:circle
                            #:with [id (geo-draw-ellipse stroke pattern)
                                       (geo-shape-extent (* 2.0 r) 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            r (for/list : (Listof Flonum) ([d (in-list diameters)])
                                (real->double-flonum d)))))

(define geo-ring
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           #:id [id : (Option Symbol) #false]
           [radius : Real-Length]
           [inner-radius : Length+% (&% 61.8)]] : Geo:Ring
    (define-values (R r) (~extent radius inner-radius))
    
    (create-geometry-object geo:ring
                            #:with [id (geo-draw-ring stroke pattern)
                                       (geo-shape-extent (* 2.0 R) 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            R r)))

(define geo-ellipse
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           #:id [id : (Option Symbol) #false]
           #:diameters [diameters : (Listof Real) null]
           [width : Real-Length] [height : Length+% (&% 61.8)]] : (U Geo:Circle Geo:Ellipse)
    (define-values (w h) (~extent width height))
    (define ellipse-extent : Geo-Calculate-Extent (geo-shape-extent w h 0.0 0.0))
    (define rads : (Listof Flonum)
      (for/list ([d (in-list diameters)])
        (real->double-flonum d)))
    
    (if (= w h)
        (create-geometry-object geo:circle
                                #:with [id (geo-draw-ellipse stroke pattern) ellipse-extent (geo-shape-outline stroke)]
                                (* w 0.5) rads)
        (create-geometry-object geo:ellipse
                                #:with [id (geo-draw-ellipse stroke pattern) ellipse-extent (geo-shape-outline stroke)]
                                (* w 0.5) (* h 0.5) rads))))

(define geo-sector
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           #:id [id : (Option Symbol) #false]
           #:ratio [ratio : Real 1.0]
           [radius : Real-Length] [start : Real] [end : Real] [unit : Angle-Unit 'rad]] : Geo:Sector
    (define ar : Nonnegative-Flonum (~dimension radius))
    (define br : Nonnegative-Flonum (if (> ratio 0.0) (abs (/ ar (real->double-flonum ratio))) ar))
    
    (create-geometry-object geo:sector
                            #:with [id (geo-draw-sector stroke pattern)
                                       (geo-shape-extent (* 2.0 ar) (* 2.0 br))
                                       (geo-shape-outline stroke)]
                            ar br (~rad start unit) (~rad end unit))))

(define geo-arc
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:id [id : (Option Symbol) #false]
           #:ratio [ratio : Real 1.0]
           [radius : Real-Length] [start : Real] [end : Real] [unit : Angle-Unit 'rad]] : Geo:Arc
    (define ar : Nonnegative-Flonum (~dimension radius))
    (define br : Nonnegative-Flonum (if (> ratio 0.0) (abs (/ ar (real->double-flonum ratio))) ar))
    
    (create-geometry-object geo:arc
                            #:with [id (geo-draw-arc stroke)
                                       (geo-shape-extent (* 2.0 ar) (* 2.0 br))
                                       (geo-shape-outline stroke)]
                            ar br (~rad start unit) (~rad end unit))))

(define geo-bullseye*
  (lambda [#:id [id : (Option Symbol) #false]
           #:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           #:eye-stroke [eye-stroke : Maybe-Stroke-Paint (void)]
           #:eye-fill [eye-pattern : Maybe-Fill-Paint (void)]
           [radius : Real-Length] [rings : (Listof Length+%)]] : Geo:Bullseye
    (define R : Nonnegative-Flonum (~dimension radius))
    
    (create-geometry-object geo:bullseye
                            #:with [id (geo-draw-bullseye stroke pattern eye-stroke eye-pattern)
                                       (geo-shape-extent (* 2.0 R) 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            R
                            (sort (for/list : (Listof Nonnegative-Flonum) ([r (in-list rings)])
                                    (abs (/ (~placement r R) R)))
                                  <))))

(define geo-bullseye
  (lambda [#:id [id : (Option Symbol) #false]
           #:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           #:eye-stroke [eye-stroke : Maybe-Stroke-Paint (void)]
           #:eye-fill [eye-pattern : Maybe-Fill-Paint (void)]
           [radius : Real-Length] . [rings : Length+% *]] : Geo:Bullseye
    (geo-bullseye* #:id id #:stroke stroke #:fill pattern
                   #:eye-stroke eye-stroke #:eye-fill eye-pattern
                   radius rings)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-draw-ellipse : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill]
    (λ [self cr x0 y0 width height]
      (define pen (geo-select-stroke-paint alt-stroke))
      (define brush (geo-select-fill-source alt-fill))
        
      (cond [(geo:ellipse? self) (dc_ellipse cr x0 y0 width height pen brush (geo:ellipse-diameters self))]
            [(geo:circle? self)  (dc_ellipse cr x0 y0 width height pen brush (geo:circle-diameters self))]))))

(define geo-draw-ring : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill]
    (λ [self cr x0 y0 width height]
      (when (geo:ring? self)
        (define pen (geo-select-stroke-paint alt-stroke))
        (define brush (geo-select-fill-source alt-fill))
        (define R (geo:ring-oradius self))
        (define r (geo:ring-iradius self))

        (if (>= r R)
            (dc_ellipse cr x0 y0 width height pen brush null)
            (dc_ring cr x0 y0 width height pen brush (abs (/ r R))))))))

(define geo-draw-bullseye : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill eye-stroke eye-fill]
    (λ [self cr x0 y0 width height]
      (when (geo:bullseye? self)
        (define pen (geo-select-stroke-paint alt-stroke))
        (define brush (geo-select-fill-source alt-fill))
        (define rings (geo:bullseye-rings% self))

        (if (pair? rings)
            (dc_bullseye cr x0 y0 width height pen brush
                         (if (void? eye-stroke) pen (geo-select-stroke-paint eye-stroke))
                         (if (void? eye-fill) brush (geo-select-fill-source eye-fill))
                         rings)
            (dc_ellipse cr x0 y0 width height pen brush null))))))

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

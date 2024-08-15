#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require "paint.rkt")
(require "../convert.rkt")
(require "../unsafe/dc/shape.rkt")
(require "../../paint.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:circle geo
  ([radius : Nonnegative-Flonum])
  #:type-name Geo:Circle
  #:transparent)

(struct geo:ellipse geo
  ([a : Nonnegative-Flonum]
   [b : Nonnegative-Flonum])
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
(define geo-circle : (-> Real [#:id (Option Symbol)] [#:border Maybe-Stroke-Paint] [#:fill Maybe-Fill-Paint] Geo:Circle)
  (lambda [radius #:id [id #false] #:border [border (void)] #:fill [pattern (void)]]
    (define r : Nonnegative-Flonum (~length radius))
    (define circle-bbox : Geo-Calculate-BBox (geo-shape-plain-bbox (* 2.0 r)))
    
    (create-geometry-object geo:circle
                            #:with [(geo-shape-surface-wrapper geo-circle-surface border pattern) circle-bbox] #:id id
                            r)))

(define geo-ellipse : (->* (Real) (Real #:id (Option Symbol) #:border Maybe-Stroke-Paint #:fill Maybe-Fill-Paint) (U Geo:Circle Geo:Ellipse))
  (lambda [width [height -0.618] #:id [id #false] #:border [border (void)] #:fill [pattern (void)]]
    (define-values (w h) (~size width height))
    (define ellipse-bbox : Geo-Calculate-BBox (geo-shape-plain-bbox w h))
    
    (if (= w h)
        (create-geometry-object geo:circle
                                #:with [(geo-shape-surface-wrapper geo-circle-surface border pattern) ellipse-bbox] #:id id
                                (* w 0.5))
        (create-geometry-object geo:ellipse
                                #:with [(geo-shape-surface-wrapper geo-ellipse-surface border pattern) ellipse-bbox] #:id id
                                (* w 0.5) (* h 0.5)))))

(define geo-sector : (->* (Real Real Real)
                          (#:id (Option Symbol) #:ratio Real #:border Maybe-Stroke-Paint #:fill Maybe-Fill-Paint #:radian? Boolean)
                          Geo:Sector)
  (lambda [#:id [id #false] #:ratio [ratio 1.0] #:border [border (void)] #:fill [pattern (void)] #:radian? [radian? #true]
           radius start end]
    (define ar : Nonnegative-Flonum (~length radius))
    (define br : Nonnegative-Flonum (if (> ratio 0.0) (abs (/ ar (real->double-flonum ratio))) ar))
    (define sector-bbox : Geo-Calculate-BBox (geo-shape-plain-bbox (* 2.0 ar) (* 2.0 br)))
    
    (create-geometry-object geo:sector
                            #:with [(geo-shape-surface-wrapper geo-sector-surface border pattern) sector-bbox] #:id id
                            ar br (~radian start radian?) (~radian end radian?))))

(define geo-arc : (->* (Real Real Real) (#:id (Option Symbol) #:ratio Real #:stroke Maybe-Stroke-Paint #:radian? Boolean) Geo:Arc)
  (lambda [#:id [id #false] #:ratio [ratio 1.0] #:stroke [stroke (void)] #:radian? [radian? #true]
           radius start end]
    (define ar : Nonnegative-Flonum (~length radius))
    (define br : Nonnegative-Flonum (if (> ratio 0.0) (abs (/ ar (real->double-flonum ratio))) ar))
    (define arc-bbox : Geo-Calculate-BBox (geo-shape-plain-bbox (* 2.0 ar) (* 2.0 br)))
    
    (create-geometry-object geo:arc
                            #:with [(geo-shape-surface-wrapper geo-arc-surface stroke) arc-bbox] #:id id
                            ar br (~radian start radian?) (~radian end radian?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-circle-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:circle?])
      (dc_circle create-abstract-surface
                 (geo:circle-radius self)
                 (current-border-source) (current-fill-source)
                 (default-geometry-density)))))

(define geo-ellipse-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:ellipse?])
      (dc_ellipse create-abstract-surface
                  (* (geo:ellipse-a self) 2.0) (* (geo:ellipse-b self) 2.0)
                  (current-border-source) (current-fill-source)
                  (default-geometry-density)))))

(define geo-arc-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:arc?])
      (define-values (ar br) (values (geo:arc-aradius self) (geo:arc-bradius self)))
      (define-values (srad erad) (values (geo:arc-start self) (geo:arc-end self)))

      (dc_arc create-abstract-surface
              (geo:arc-aradius self) (geo:arc-bradius self)
              (geo:arc-start self) (geo:arc-end self)
              (current-stroke-source*)
              (default-geometry-density)))))

(define geo-sector-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:sector?])
      (define-values (ar br) (values (geo:sector-aradius self) (geo:sector-bradius self)))
      (define-values (srad erad) (values (geo:sector-start self) (geo:sector-end self)))

      (dc_sector create-abstract-surface ar br srad erad
                 (current-border-source) (current-fill-source)
                 (default-geometry-density)))))
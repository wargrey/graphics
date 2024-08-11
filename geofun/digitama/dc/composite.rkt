#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/unsafe/visual/ctype)

(require "../source.rkt")
(require "../convert.rkt")
(require "../composite.rkt")
(require "../unsafe/composite.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:group geo
  ([composition-operator : (Option Symbol)]
   [layers : Geo-Layers])
  #:type-name Geo:Group
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-composite : (case-> [Symbol Geo<%> Complex Geo<%> -> Geo:Group]
                                [Symbol Geo<%> Real Real Geo<%> -> Geo:Group]
                                [Symbol Geo<%> Complex Geo<%> Complex -> Geo:Group]
                                [Symbol Geo<%> Real Real Geo<%> Real Real -> Geo:Group])
  (case-lambda
    [(op geo1 x/pt y/geo2 geo2/pt)
     (if (real? y/geo2)
         (geo-composite op geo1 x/pt y/geo2 geo2/pt 0.0 0.0)
         (geo-composite op geo1 (- x/pt geo2/pt) y/geo2))]
    [(op geo1 pt geo2)
     (make-geo:group op
                     (geo-composite-layers geo1 geo2
                                           (real->double-flonum (real-part pt))
                                           (real->double-flonum (imag-part pt))))]
    [(op geo1 x1 y1 geo2 x2 y2)
     (make-geo:group op
                     (geo-composite-layers geo1 geo2
                                           (- (real->double-flonum x1) (real->double-flonum x2))
                                           (- (real->double-flonum y1) (real->double-flonum y2))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-composite-layers : (case-> [Geo<%> Geo<%> Flonum Flonum -> Geo-Layers]
                                       [Geo<%> Geo<%> Flonum Flonum Flonum Flonum -> Geo-Layers]
                                       [Geo<%> Geo<%> Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum -> Geo-Layers])
  (case-lambda
    [(geo1 geo2 dx dy)
     (let-values ([(w1 h1) (geo-size geo1)]
                  [(w2 h2) (geo-size geo2)])
       (geo-composite-layers geo1 geo2 w1 h1 w2 h2 dx dy))]
    [(geo1 geo2 x1% y1% x2% y2%)
     (let-values ([(w1 h1) (geo-size geo1)]
                  [(w2 h2) (geo-size geo2)])
       (geo-composite-layers geo1 geo2 w1 h1 w2 h2
                             (- (* x1% w1) (* x2% w2))
                             (- (* y1% h1) (* y2% h2))))]
    [(geo1 geo2 w1 h1 w2 h2 dx dy)
     (let-values ([(dx1 dy1) (values (max (- 0.0 dx) 0.0) (max (- 0.0 dy) 0.0))]
                  [(dx2 dy2) (values (max dx 0.0) (max dy 0.0))])
       (define width  (max (+ dx1 w1) (+ dx2 w2)))
       (define height (max (+ dy1 h1) (+ dy2 h2)))

       (vector-immutable width height
                         (list (vector-immutable geo1 dx1 dy1 w1 h1)
                               (vector-immutable geo2 dx2 dy2 w2 h2))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-geo:group : (-> Symbol Geo-Layers Geo:Group)
  (lambda [op layers]
    (create-geometry-object geo:group
                            #:with [geo-group-surface (geo-group-bbox layers)] #:id #false
                            op layers)))

(define geo-group-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:group?])
      (define op (geo:group-composition-operator self))
      (define op-seq (and op ))
      (geo_composite (geo-operator->integer* (geo:group-composition-operator self)
                                             (default-composition-operator))
                     (geo:group-layers self) 
                     (default-geometry-density)))))

(define geo-group-bbox : (-> Geo-Layers Geo-Calculate-BBox)
  (lambda [layers]
    (define w (vector-ref layers 0))
    (define h (vector-ref layers 1))
    
    (λ [self]
      (values 0.0 0.0 w h))))

#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/unsafe/visual/ctype)

(require "../../paint.rkt")

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
(define geo-composite : (->* (Geo<%> Real Real Geo<%>) (Real Real #:operator (Option Symbol) #:id (Option Symbol)) Geo:Group)
  (lambda [geo1 x1 y1 geo2 [x2 0.0] [y2 0.0] #:operator [op #false] #:id [id #false]]
    (make-geo:group id op
                    (geo-composite-layers geo1 geo2
                                          (- (real->double-flonum x1) (real->double-flonum x2))
                                          (- (real->double-flonum y1) (real->double-flonum y2))))))

#;(define geo-pin : (-> Real Real Real Real Geo<%> [#:operator (Option Symbol)] [#:id (Option Symbol)] Geo<%> * Geo<%>)
  (lambda [#:operator [op #false] #:id [id #false] x1% y1% x2% y2% base . geos]
    (cond [(null? geos) base]
          [(null? (cdr geos))
           (make-geo:group op
                           (geo-composite-layers geo1 geo2
                                                 (real->double-flonum (real-part pt))
                                                 (real->double-flonum (imag-part pt))))])
    (define-values (min-width min-height) (geo-size base))
    (define-values (flwidth flheight all)
      (let compose ([lla (list (vector sfc1 0.0 0.0 min-width min-height))]
                    [width min-width] [height min-height] 
                    [dx 0.0] [dy 0.0]  ; offsets passed to (bitmap_composite), also see (flomap-pin*)
                    [width1 min-width] [height1 min-height] [children sfcs])
        (cond [(null? children) (values width height (reverse lla))]
              [else (let ([sfc2 (unsafe-car children)])
                      (define-values (width2 height2) (bitmap-surface-rendered-size sfc2 density))
                      (define nx (unsafe-fl+ dx (unsafe-fl- (unsafe-fl* width1 x1%) (unsafe-fl* width2 x2%))))
                      (define ny (unsafe-fl+ dy (unsafe-fl- (unsafe-fl* height1 y1%) (unsafe-fl* height2 y2%))))
                      (define-values (xoff1 yoff1) (values (unsafe-flmax (unsafe-fl- 0.0 nx) 0.0) (unsafe-flmax (unsafe-fl- 0.0 ny) 0.0)))
                      (define-values (xoff2 yoff2) (values (unsafe-flmax nx 0.0) (unsafe-flmax ny 0.0)))

                      (compose (unsafe-cons-list (vector sfc2 xoff2 yoff2 width2 height2) lla)
                               (unsafe-flmax (unsafe-fl+ xoff1 width) (unsafe-fl+ xoff2 width2))
                               (unsafe-flmax (unsafe-fl+ yoff1 height) (unsafe-fl+ yoff2 height2))
                               (unsafe-flmax nx 0.0) (unsafe-flmax ny 0.0) width2 height2
                               (unsafe-cdr children)))])))
    
    (define-values (bmp cr) (create-argb-bitmap flwidth flheight density #true))
    (define operator (or operator0 CAIRO_OPERATOR_OVER))
    (let combine ([all all])
      (unless (null? all)
        (define child (unsafe-car all))
        (cairo-composite cr (unsafe-vector*-ref child 0)
                         (unsafe-vector*-ref child 1) (unsafe-vector*-ref child 2)
                         (unsafe-vector*-ref child 3) (unsafe-vector*-ref child 4)
                         CAIRO_FILTER_BILINEAR operator density)
        (combine (unsafe-cdr all))))
    bmp))

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
(define make-geo:group : (-> (Option Symbol) (Option Symbol) Geo-Layers Geo:Group)
  (lambda [id op layers]
    (create-geometry-object geo:group
                            #:with [geo-group-surface (geo-group-bbox layers)] #:id id
                            op layers)))

(define geo-group-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:group?])
      (geo_composite (geo-select-operator (geo:group-composition-operator self)
                                          (default-pin-operator))
                     (geo:group-layers self) 
                     (default-geometry-density)))))

(define geo-group-bbox : (-> Geo-Layers Geo-Calculate-BBox)
  (lambda [layers]
    (define w (vector-ref layers 0))
    (define h (vector-ref layers 1))
    
    (λ [self]
      (values 0.0 0.0 w h))))

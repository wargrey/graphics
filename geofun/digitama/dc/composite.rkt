#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/unsafe/visual/ctype)

(require "../../paint.rkt")

(require "../convert.rkt")
(require "../composite.rkt")
(require "../unsafe/composite.rkt")

;;; TODO
; deal with compositions with negative parameters.
; `flomap` will recalculate the locations,
; `pict` simply drop parts outside boundary.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:group geo
  ([composition-operator : (Option Symbol)]
   [layers : Geo-Layer-Group])
  #:type-name Geo:Group
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-composite : (->* (Geo<%> Real Real Geo<%>) (Real Real #:operator (Option Symbol) #:id (Option Symbol)) Geo:Group)
  (lambda [geo1 x1 y1 geo2 [x2 0.0] [y2 0.0] #:operator [op #false] #:id [id #false]]
    (make-geo:group id op
                    (geo-composite-layers geo1 geo2
                                          (- (real->double-flonum x1) (real->double-flonum x2))
                                          (- (real->double-flonum y1) (real->double-flonum y2))))))

(define geo-pin* : (-> Real Real Real Real Geo<%> [#:operator (Option Symbol)] [#:id (Option Symbol)] Geo<%> * Geo<%>)
  (lambda [#:operator [op #false] #:id [id #false] x1% y1% x2% y2% base . children]
    (cond [(null? children) base]
          [(null? (cdr children))
           (make-geo:group id op
                           (geo-composite-layers base (car children)
                                                 (real->double-flonum x1%) (real->double-flonum y1%)
                                                 (real->double-flonum x2%) (real->double-flonum y2%)))]
          [else (let-values ([(min-width min-height) (geo-size base)])
                  (make-geo:group id op
                                  (let pin ([sreyal : (Listof Geo-Layer) null]
                                            [width : Nonnegative-Flonum min-width] [height : Nonnegative-Flonum min-height] 
                                            [dx : Flonum 0.0] [dy : Flonum 0.0]
                                            [w1 : Nonnegative-Flonum min-width] [h1 : Nonnegative-Flonum min-height]
                                            [siblings : (Listof Geo<%>) children])
                                    (if (pair? siblings)
                                        (let ([geo2 (car children)])
                                          (define-values (w2 h2) (geo-size geo2))
                                          (define nx : Flonum (+ dx (- (* w1 x1%) (* w2 x2%))))
                                          (define ny : Flonum (+ dy (- (* h1 y1%) (* h2 y2%))))
                                          (define-values (xoff1 yoff1) (values (max (- 0.0 nx) 0.0) (max (- 0.0 ny) 0.0)))
                                          (define-values (xoff2 yoff2) (values (max nx 0.0) (max ny 0.0)))
                                          
                                          (pin (cons (vector-immutable geo2 xoff2 yoff2 w2 h2) sreyal)
                                               (max (+ xoff1 width) (+ xoff2 w2)) (max (+ yoff1 height) (+ yoff2 h2))
                                               (max nx 0.0) (max ny 0.0) w2 h2 (cdr siblings)))
                                        (vector-immutable width height
                                                          (cons (vector-immutable base 0.0 0.0 min-width min-height)
                                                                (reverse sreyal)))))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-composite-layers : (case-> [Geo<%> Geo<%> Flonum Flonum -> Geo-Layer-Group]
                                       [Geo<%> Geo<%> Flonum Flonum Flonum Flonum -> Geo-Layer-Group]
                                       [Geo<%> Geo<%> Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum -> Geo-Layer-Group])
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
(define make-geo:group : (-> (Option Symbol) (Option Symbol) Geo-Layer-Group Geo:Group)
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

(define geo-group-bbox : (-> Geo-Layer-Group Geo-Calculate-BBox)
  (lambda [layers]
    (define w (vector-ref layers 0))
    (define h (vector-ref layers 1))
    
    (λ [self]
      (values 0.0 0.0 w h))))

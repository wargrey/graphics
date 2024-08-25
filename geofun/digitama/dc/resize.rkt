#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)
(require bitmap/digitama/convert)
(require bitmap/digitama/unsafe/image)

(require "../base.rkt")

(require "../convert.rkt")
(require "../unsafe/resize.rkt")
(require "../unsafe/visual/ctype.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:transform geo
  ([source : Geo])
  #:type-name Geo:Transform
  #:transparent)

(struct geo:region geo:transform
  ([x : Flonum]
   [y : Flonum]
   [width : Flonum]
   [height : Flonum])
  #:type-name Geo:Region
  #:transparent)

(struct geo:scale geo:transform
  ([sx : Flonum]
   [sy : Flonum])
  #:type-name Geo:Scale
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-scale : (->* (Geo Real) (Real) Geo)
  (case-lambda
    [(self s) (if (= s 1.0) self (geo-scale self s s))]
    [(self sx sy)
     (cond [(and (= sx 1.0) (= sy 1.0)) self]
           [(geo:scale? self) (geo-scale (geo:transform-source self) (* sx (geo:scale-sx self)) (* sy (geo:scale-sy self)))]
           [else (let ([flsx (real->double-flonum sx)]
                       [flsy (real->double-flonum sy)])
                   (create-geometry-object geo:scale
                                           #:surface geo-scale-surface #:bbox (geo-scale-bbox flsx flsy) #:id (geo-id self)
                                           self flsx flsy))])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-geo:region : (-> Geo Real Real Nonnegative-Real Nonnegative-Real Geo:Region)
  (lambda [self x y width height]
    (define flx (real->double-flonum x))
    (define fly (real->double-flonum y))
    (define flw (real->double-flonum width))
    (define flh (real->double-flonum height))

    (create-geometry-object geo:region
                            #:surface geo-region-surface #:bbox (geo-shape-plain-bbox flw flh) #:id (geo-id self)
                            self flx fly flw flh)))

(define geo-region-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:region?])
      (geo_section (geo-create-surface (geo:transform-source self))
                   (geo:region-x self) (geo:region-y self)
                   (geo:region-width self) (geo:region-height self)
                   (default-geometry-density)))))

(define geo-scale-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:scale?])
      (geo_scale (geo-create-surface (geo:transform-source self))
                 (geo:scale-sx self) (geo:scale-sy self)
                 (default-geometry-density)))))

(define geo-scale-bbox : (-> Flonum Flonum Geo-Calculate-BBox)
  (lambda [sx sy]
    (λ [self]
      (with-asserts ([self geo:scale?])
        (define-values (asx asy) (values (abs sx) (abs sy)))
        (define-values (ox oy owidth oheight) (geo-bounding-box (geo:transform-source self)))
        (values (if (> sx 0.0) (* sx ox) 0.0)
                (if (> sy 0.0) (* sy oy) 0.0)
                (* asx owidth) (* asy oheight))))))

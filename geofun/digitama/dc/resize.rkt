#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require "ink.rkt")

(require "../convert.rkt")
(require "../unsafe/adjust.rkt")
(require "../geometry/affine.rkt")

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

(struct geo:scaling geo:transform
  ([sx : Flonum]
   [sy : Flonum])
  #:type-name Geo:Scaling
  #:transparent)

(struct geo:rotation geo:transform
  ([theta : Flonum])
  #:type-name Geo:Rotation
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-scale : (->* (Geo Real) (Real) Geo)
  (case-lambda
    [(self s) (if (= s 1.0) self (geo-scale self s s))]
    [(self sx sy)
     (cond [(and (= sx 1.0) (= sy 1.0)) self]
           [(geo:scaling? self) (geo-scale (geo:transform-source self) (* sx (geo:scaling-sx self)) (* sy (geo:scaling-sy self)))]
           [else (create-geometry-object geo:scaling
                                         #:surface geo-scaling-surface #:extent geo-scaling-extent #:id (geo-id self)
                                         self (real->double-flonum sx) (real->double-flonum sy))])]))

(define geo-rotate : (->* (Geo Real) (Boolean) Geo)
  (lambda [self theta [radian? #true]]
    (define fltheta : Flonum (~radian theta radian?))
    (cond [(= fltheta 0.0) self]
          [(geo:rotation? self) (geo-rotate (geo:transform-source self) (+ fltheta (geo:rotation-theta self)))]
          [else (create-geometry-object geo:rotation
                                        #:surface geo-rotation-surface #:extent geo-rotation-extent #:id (geo-id self)
                                        self fltheta)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-geo:region : (-> Geo Real Real Nonnegative-Real Nonnegative-Real Geo:Region)
  (lambda [self x y width height]
    (define flx (real->double-flonum x))
    (define fly (real->double-flonum y))
    (define flw (real->double-flonum width))
    (define flh (real->double-flonum height))

    (create-geometry-object geo:region
                            #:surface geo-region-surface #:extent (geo-shape-plain-extent flw flh) #:id (geo-id self)
                            self flx fly flw flh)))

(define geo-region-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:region?])
      (geo_section (geo-create-surface (geo:transform-source self))
                   (geo:region-x self) (geo:region-y self)
                   (geo:region-width self) (geo:region-height self)
                   (default-geometry-density)))))

(define geo-scaling-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:scaling?])
      (geo_scale (geo-create-surface (geo:transform-source self))
                 (geo:scaling-sx self) (geo:scaling-sy self)
                 (default-geometry-density)))))

(define geo-scaling-extent : Geo-Calculate-Extent
  (lambda [self]
    (with-asserts ([self geo:scaling?])
      (define-values (sx sy) (values (geo:scaling-sx self) (geo:scaling-sy self)))
      (define-values (owidth oheight ?oink) (geo-extent (geo:transform-source self)))
      (values (* (abs sx) owidth) (* (abs sy) oheight)
              (and ?oink (geo-ink-scale ?oink sx sy))))))

(define geo-rotation-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:rotation?])
      (geo_rotate (geo-create-surface (geo:transform-source self)) (geo:rotation-theta self)
                  (default-geometry-density)))))

(define geo-rotation-extent : Geo-Calculate-Extent
  (lambda [self]
    (with-asserts ([self geo:rotation?])
      (define-values (owidth oheight ?oink) (geo-extent (geo:transform-source self)))
      (define-values (rw rh) (geo-size-rotate owidth oheight (geo:rotation-theta self)))
      (values rw rh (and ?oink (geo-ink-rotate ?oink (geo:rotation-theta self)))))))

#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require "../convert.rkt")
(require "../unsafe/adjust.rkt")

(require "../geometry/ink.rkt")
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
                                         #:with [(geo-id self) geo-draw/scaling! geo-scaling-extent]
                                         self (real->double-flonum sx) (real->double-flonum sy))])]))

(define geo-rotate : (->* (Geo Real) (Boolean) Geo)
  (lambda [self theta [radian? #true]]
    (define fltheta : Flonum (~radian theta radian?))
    (cond [(= fltheta 0.0) self]
          [(geo:rotation? self) (geo-rotate (geo:transform-source self) (+ fltheta (geo:rotation-theta self)))]
          [else (create-geometry-object geo:rotation
                                        #:with [(geo-id self) geo-draw/rotation! geo-rotation-extent]
                                        self fltheta)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-geo:region : (-> Geo Real Real Nonnegative-Real Nonnegative-Real Geo:Region)
  (lambda [self x y width height]
    (define flx (real->double-flonum x))
    (define fly (real->double-flonum y))
    (define flw (real->double-flonum width))
    (define flh (real->double-flonum height))

    (create-geometry-object geo:region
                            #:with [(geo-id self) geo-draw/region! (geo-shape-plain-extent flw flh)]
                            self flx fly flw flh)))

(define geo-draw/region! : Geo-Surface-Draw!
  (lambda [self dc x0 y0 width height]
    (with-asserts ([self geo:region?])
      (define master : Geo<%> (geo:transform-source self))
      (define-values (src-width src-height) (geo-intrinsic-size master))
      
      (geo_section dc x0 y0 width height
                   (geo:region-x self) (geo:region-y self)
                   (geo<%>-draw! master) master src-width src-height))))

(define geo-draw/scaling! : Geo-Surface-Draw!
  (lambda [self dc x0 y0 width height]
    (with-asserts ([self geo:scaling?])
      (define master : Geo<%> (geo:transform-source self))
      (define-values (src-width src-height) (geo-intrinsic-size master))

      (geo_scale dc x0 y0 width height
                 (geo:scaling-sx self) (geo:scaling-sy self)
                 (geo<%>-draw! master) master src-width src-height))))

(define geo-scaling-extent : Geo-Calculate-Extent
  (lambda [self]
    (with-asserts ([self geo:scaling?])
      (define-values (sx sy) (values (geo:scaling-sx self) (geo:scaling-sy self)))
      (define-values (owidth oheight ?oink) (geo-extent (geo:transform-source self)))
      (values (* (abs sx) owidth) (* (abs sy) oheight)
              (and ?oink (geo-ink-scale ?oink sx sy))))))

(define geo-draw/rotation! : Geo-Surface-Draw!
  (lambda [self dc x0 y0 width height]
    (with-asserts ([self geo:rotation?])
      (define master : Geo<%> (geo:transform-source self))
      (define-values (src-width src-height) (geo-intrinsic-size master))

      (geo_rotate dc x0 y0 width height
                  (geo:rotation-theta self)
                  (geo<%>-draw! master) master src-width src-height))))

(define geo-rotation-extent : Geo-Calculate-Extent
  (lambda [self]
    (with-asserts ([self geo:rotation?])
      (define-values (owidth oheight ?oink) (geo-extent (geo:transform-source self)))
      (define-values (rw rh) (geo-size-rotate owidth oheight (geo:rotation-theta self)))
      (values rw rh (and ?oink (geo-ink-rotate ?oink (geo:rotation-theta self)))))))

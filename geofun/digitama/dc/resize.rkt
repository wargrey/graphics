#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require "../self.rkt")
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

(struct geo:scale geo:transform
  ([sx : Flonum]
   [sy : Flonum])
  #:type-name Geo:Scale
  #:transparent)

(struct geo:rotation geo:transform
  ([theta : Flonum])
  #:type-name Geo:Rotation
  #:transparent)

(struct geo:shear geo:transform
  ([shx : Flonum]
   [shy : Flonum])
  #:type-name Geo:Shear
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-scale : (->* (Geo Real) (Real) Geo)
  (case-lambda
    [(self s) (geo-scale self s s)] 
    [(self sx0 sy0)
     (let-values ([(sx sy) (values (real->double-flonum sx0) (real->double-flonum sy0))])
       (cond [(and (= sx 1.0) (= sy 1.0)) self]
             [(not (geo:scale? self))
              (create-geometry-object geo:scale
                                      #:with [(geo-id self) geo-draw/scale!
                                                            (geo-delegate-expand self sx sy)
                                                            (geo-pad-scale (geo-outline self) self sx sy)]
                                      self sx sy)]
             [else (geo-scale (geo:transform-source self) (* sx (geo:scale-sx self)) (* sy (geo:scale-sy self)))]))]))

(define geo-rotate : (->* (Geo Real) (Boolean) Geo)
  (lambda [self theta [radian? #true]]
    (define fltheta : Flonum (~radian theta radian?))
    (cond [(= fltheta 0.0) self]
          [(geo:rotation? self) (geo-rotate (geo:transform-source self) (+ fltheta (geo:rotation-theta self)))]
          [else (create-geometry-object geo:rotation
                                        #:with [(geo-id self) geo-draw/rotation! geo-rotation-extent (geo-outline self)]
                                        self fltheta)])))

(define geo-shear : (-> Geo Real Real Geo)
  (lambda [self shx shy]
    (define fx : Flonum (real->double-flonum shx))
    (define fy : Flonum (real->double-flonum shy))
    
    (cond [(and (= fx 0.0) (= fy 0.0)) self]
          [(geo:shear? self) (geo-shear (geo:transform-source self) (+ fx (geo:shear-shx self)) (+ fy (geo:shear-shy self)))]
          [else (create-geometry-object geo:shear
                                        #:with [(geo-id self) geo-draw/shear! geo-shear-extent (geo-outline self)]
                                        self fx fy)])))

(define geo-skew : (->* (Geo Real Real) (Boolean) Geo)
  (lambda [self skx sky [radian? #true]]
    (geo-shear self
               (tan (~radian skx radian?))
               (tan (~radian sky radian?)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-geo:region : (-> Geo Real Real Nonnegative-Real Nonnegative-Real Geo:Region)
  (lambda [self x y width height]
    (define flx (real->double-flonum x))
    (define fly (real->double-flonum y))
    (define flw (real->double-flonum width))
    (define flh (real->double-flonum height))

    (create-geometry-object geo:region
                            #:with [(geo-id self) geo-draw/region! (geo-shape-extent flw flh) (geo-outline self)]
                            self flx fly flw flh)))

(define geo-draw/region! : Geo-Surface-Draw!
  (lambda [self dc x0 y0 width height]
    (with-asserts ([self geo:region?])
      (define master : Geo<%> (geo:transform-source self))
      (define-values (src-width src-height) (geo-intrinsic-size master))
      
      (geo_section dc x0 y0 width height
                   (geo:region-x self) (geo:region-y self)
                   (geo<%>-draw! master) master src-width src-height))))

(define geo-draw/scale! : Geo-Surface-Draw!
  (lambda [self dc x0 y0 width height]
    (with-asserts ([self geo:scale?])
      (define master : Geo<%> (geo:transform-source self))
      (define-values (src-width src-height) (geo-intrinsic-size master))

      (geo_scale dc x0 y0 width height
                 (geo:scale-sx self) (geo:scale-sy self)
                 (geo<%>-draw! master) master src-width src-height))))
  
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
      (values rw rh
              (and ?oink (geo-ink-rotate ?oink (geo:rotation-theta self)))))))

(define geo-draw/shear! : Geo-Surface-Draw!
  (lambda [self dc x0 y0 width height]
    (with-asserts ([self geo:shear?])
      (define master : Geo<%> (geo:transform-source self))
      (define-values (src-width src-height) (geo-intrinsic-size master))

      (geo_shear dc x0 y0 width height
                 (geo:shear-shx self) (geo:shear-shy self)
                 (geo<%>-draw! master) master src-width src-height))))

(define geo-shear-extent : Geo-Calculate-Extent
  (lambda [self]
    (with-asserts ([self geo:shear?])
      (define-values (shx shy) (values (geo:shear-shx self) (geo:shear-shy self)))
      (define-values (owidth oheight ?oink) (geo-extent (geo:transform-source self)))

      (values (+ (* (abs shx) oheight) owidth)
              (+ (* (abs shy) owidth)  oheight)
              (and ?oink (geo-ink-shear ?oink shx shy))))))

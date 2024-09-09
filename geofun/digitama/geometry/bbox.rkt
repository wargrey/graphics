#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo-bbox
  ([lx : Flonum]
   [ty : Flonum]
   [rx : Flonum]
   [by : Flonum])
  #:constructor-name unsafe-geo-bbox
  #:type-name Geo-BBox
  #:transparent
  #:mutable)

(define make-geo-bbox : (case-> [Real Real Real Real -> Geo-BBox]
                                [Complex Complex -> Geo-BBox]
                                [Complex -> Geo-BBox])
  (case-lambda
    [(lx ty rx by) (unsafe-geo-bbox (real->double-flonum lx) (real->double-flonum ty) (real->double-flonum rx) (real->double-flonum by))]
    [(lpt rpt) (make-geo-bbox (real-part lpt) (imag-part lpt) (real-part rpt) (imag-part rpt))]
    [(pt) (make-geo-bbox (real-part pt) (imag-part pt) (real-part pt) (imag-part pt))]))

(define geo-bbox-fit! : (case-> [Geo-BBox Float-Complex -> Void]
                                [Geo-BBox Float-Complex Flonum Flonum -> Void]
                                [Geo-BBox Flonum Flonum -> Void])
  (case-lambda
    [(self pos) (geo-bbox-fit! self (real-part pos) (imag-part pos))]
    [(self pos dx dy) (geo-bbox-fit! self (+ (real-part pos) dx) (+ (imag-part pos) dy))]
    [(self x y)
     (cond [(< x (geo-bbox-lx self)) (set-geo-bbox-lx! self x)]
           [(> x (geo-bbox-rx self)) (set-geo-bbox-rx! self x)])
     (cond [(< y (geo-bbox-ty self)) (set-geo-bbox-ty! self y)]
           [(> y (geo-bbox-by self)) (set-geo-bbox-by! self y)])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-bbox-position : (-> Geo-BBox Float-Complex)
  (lambda [self]
    (make-rectangular (geo-bbox-lx self) (geo-bbox-ty self))))

(define geo-bbox-offset : (-> Geo-BBox Float-Complex)
  (lambda [self]
    (make-rectangular (- (geo-bbox-lx self)) (- (geo-bbox-ty self)))))

(define geo-bbox-position-values : (-> Geo-BBox (Values Flonum Flonum))
  (lambda [self]
    (values (geo-bbox-lx self) (geo-bbox-ty self))))

(define geo-bbox-offset-values : (-> Geo-BBox (Values Flonum Flonum))
  (lambda [self]
    (values (- (geo-bbox-lx self)) (- (geo-bbox-ty self)))))

(define geo-bbox-values : (-> Geo-BBox (Values Nonnegative-Flonum Nonnegative-Flonum Float-Complex))
  (lambda [self]
    (define-values (lx ty) (values (geo-bbox-lx self) (geo-bbox-ty self)))
    (define-values (rx by) (values (geo-bbox-rx self) (geo-bbox-by self)))

    (values (max (- rx lx) 0.0)
            (max (- by ty) 0.0)
            (make-rectangular lx ty))))

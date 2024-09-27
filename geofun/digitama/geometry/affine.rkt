#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-size-rotate : (-> Nonnegative-Flonum Nonnegative-Flonum Flonum (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [w h rad]
    (define-values (ucos usin) (values (abs (cos rad)) (abs (sin rad))))
    (define-values (wcos wsin) (values (* w ucos) (* w usin)))
    (define-values (hcos hsin) (values (* h ucos) (* h usin)))
    
    (values (+ wcos hsin) (+ wsin hcos))))

(define geo-point-rotate : (-> Flonum Flonum Flonum Flonum Flonum (Values Flonum Flonum))
  (lambda [x y ox oy rad]
    (define-values (cosr sinr) (values (cos rad) (sin rad)))
    (define-values (dx dy) (values (- x ox) (- y oy)))
    
    (values (+ (* dx cosr) (- (* dy sinr)) ox)
            (+ (* dx sinr) (+ (* dy cosr)) oy))))

(define geo-rectangle-rotate : (-> Nonnegative-Flonum Nonnegative-Flonum Flonum (Values Flonum Flonum Flonum Flonum))
  (lambda [w h rad]
    (define-values (cx cy) (values (* w 0.5) (* h 0.5)))
    (define R (make-polar 1.0 rad))
    (define O (make-rectangular cx cy))
    (define A (* (- (make-rectangular 0.0 0.0) O) R))
    (define B (* (- (make-rectangular   w 0.0) O) R))
    (define C (* (- (make-rectangular   w   h) O) R))
    (define D (* (- (make-rectangular 0.0   h) O) R))

    (let interval ([rmin : Flonum +inf.0]
                   [rmax : Flonum -inf.0]
                   [imin : Flonum +inf.0]
                   [imax : Flonum -inf.0]
                   [pts : (Listof Float-Complex) (list A B C D)])
      (if (pair? pts)
          (let ([r (real-part (car pts))]
                [i (imag-part (car pts))])
            (interval (min rmin r) (max rmax r)
                      (min imin i) (max imax i)
                      (cdr pts)))
          (values (+ rmin cx) (+ imin cy)
                  (+ rmax cx) (+ imax cy))))))

#lang typed/racket/base

(provide (all-defined-out) pi)

(require racket/math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-values (-pi/2 pi/2 3pi/2 2pi pi/4 3pi/4)
  (values (radians->degrees -90.0)
          (radians->degrees 90.0)
          (radians->degrees 270.0)
          (* pi 2.0)
          (* pi 0.25)
          (* pi 0.75)))

(define phi : Nonnegative-Flonum (* (+ 1.0 (sqrt 5.0)) 0.5))
(define 1/phi : Nonnegative-Flonum (/ 2.0 (+ 1.0 (sqrt 5.0))))
(define -phi : Flonum (- 0.0 phi))

(define 2√3 : Nonnegative-Flonum (* 2.0 (sqrt 3.0)))
(define 1/φ² : Nonnegative-Flonum (* 1/phi 1/phi))
(define sin72º : Nonnegative-Flonum (max (sin (* 2pi 0.2)) 0.0))
(define 1/sin72º : Nonnegative-Flonum (max (/ 1.0 sin72º) 0.0))

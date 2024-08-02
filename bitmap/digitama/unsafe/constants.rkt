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

(define phi : Nonnegative-Flonum (/ (+ 1.0 (sqrt 5.0)) 2.0))
(define -phi : Flonum (- 0.0 phi))

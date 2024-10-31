#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out digimon/constant))

(require digimon/constant)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define 2√3 : Nonnegative-Flonum (* 2.0 (sqrt 3.0)))
(define 1/φ² : Nonnegative-Flonum (* 1/phi 1/phi))
(define sin72º : Nonnegative-Flonum (max (sin 2pi/5) 0.0))
(define 1/sin72º : Nonnegative-Flonum (max (/ 1.0 sin72º) 0.0))

(define sin54º : Nonnegative-Flonum (max (cos (* pi 0.3)) 0.0))
(define cos54º : Nonnegative-Flonum (max (cos (* pi 0.3)) 0.0))
(define tan54º : Nonnegative-Flonum (max (tan (* pi 0.3)) 0.0))

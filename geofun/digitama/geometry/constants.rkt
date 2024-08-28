#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out digimon/constant))

(require digimon/constant)
(require racket/math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define 2√3 : Nonnegative-Flonum (* 2.0 (sqrt 3.0)))
(define 1/φ² : Nonnegative-Flonum (* 1/phi 1/phi))
(define sin72º : Nonnegative-Flonum (max (sin (* 2pi 0.2)) 0.0))
(define 1/sin72º : Nonnegative-Flonum (max (/ 1.0 sin72º) 0.0))

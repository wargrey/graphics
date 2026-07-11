#lang typed/racket/base

(provide (all-defined-out))

;;; The professional way of converting CMYK to RGB requires an ICC profile
;;; The default implmentation is used as a fallback when lacking of ICC profile

(require digimon/measure)

(require racket/flonum)
(require racket/unsafe/ops)

(require "digitama/cmyk.rkt")
(require "digitama/correction.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type CMYK-Ink-Config (U 'US 'Europe 'Japan CMYK-Ink-Matrix))
(define-type CMYK->RGB (-> Flonum Flonum Flonum Flonum (Values Flonum Flonum Flonum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define device-cmyk->rgb : (->* (Flonum Flonum Flonum Flonum) (CMYK-Ink-Config) (Values Flonum Flonum Flonum))
  (lambda [c m y k [type cmyk-matrix-swop]]
    (define mtx
      (cond [(vector? type) cmyk-matrix-swop]
            [(eq? type 'US) cmyk-matrix-swop]
            [(eq? type 'Europe) cmyk-matrix-euroscale]
            [(eq? type 'Japan) cmyk-matrix-japan]
            [else cmyk-matrix-swop]))
    
    (define R (+ (* c (unsafe-vector*-ref mtx 0)) (* m (unsafe-vector*-ref mtx 1)) (* y (unsafe-vector*-ref mtx 2))))
    (define G (+ (* c (unsafe-vector*-ref mtx 3)) (* m (unsafe-vector*-ref mtx 4)) (* y (unsafe-vector*-ref mtx 5))))
    (define B (+ (* c (unsafe-vector*-ref mtx 6)) (* m (unsafe-vector*-ref mtx 7)) (* y (unsafe-vector*-ref mtx 8))))
    (define r (color-component-gamma-encode (* (- 1.0 R) (- 1.0 k))))
    (define g (color-component-gamma-encode (* (- 1.0 G) (- 1.0 k))))
    (define b (color-component-gamma-encode (* (- 1.0 B) (- 1.0 k))))

    (values r g b)))

#lang typed/racket

(provide (all-defined-out) select-color)

(require "digitama/digicore.rkt")
(require "digitama/color.rkt")
(require "constructor.rkt")

(require typed/images/icons)

(define bitmap-size : (case-> [Bitmap -> (Values Positive-Integer Positive-Integer)]
                              [Bitmap Nonnegative-Real -> (Values Nonnegative-Real Nonnegative-Real)]
                              [Bitmap Nonnegative-Real Nonnegative-Real -> (Values Nonnegative-Real Nonnegative-Real)])
  (case-lambda [(bmp) (values (send bmp get-width) (send bmp get-height))]
               [(bmp ratio) (values (* (send bmp get-width) ratio) (* (send bmp get-height) ratio))]
               [(bmp w-ratio h-ratio) (values (* (send bmp get-width) w-ratio) (* (send bmp get-height) h-ratio))]))

(define bitmap-size+density : (Bitmap -> (Values Positive-Integer Positive-Integer Positive-Real))
  (lambda [bmp]
    (values (send bmp get-width)
            (send bmp get-height)
            (send bmp get-backing-scale))))

(define bitmap-intrinsic-size : (-> Bitmap (Values Positive-Integer Positive-Integer))
  (lambda [bmp]
    (define density : Positive-Real (send bmp get-backing-scale))
    (values (max (exact-ceiling (* (send bmp get-width) density)) 1)
            (max (exact-ceiling (* (send bmp get-height) density)) 1))))

(define bitmap-alter-density : (->* (Bitmap) (Positive-Real) Bitmap)
  (lambda [raw [density (default-icon-backing-scale)]]
    (define ratio : Nonnegative-Real (/ (send raw get-backing-scale) density))
    (cond [(= ratio 1.0) raw]
          [else (let ([bmp (bitmap-blank (* (send raw get-width) ratio) (* (send raw get-height) ratio) density)])
                  ; This algorithm is much faster than the (get/set-argb-pixels) one
                  (define dc : (Instance Bitmap-DC%) (send bmp make-dc))
                  (send dc set-smoothing 'aligned)
                  (send dc set-scale ratio ratio)
                  (send dc draw-bitmap raw 0 0)
                  bmp)])))

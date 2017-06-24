#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/digicore.rkt")
(require "digitama/unsafe/bitmap.rkt")

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

(define bitmap-alter-density : (->* (Bitmap) (Positive-Flonum) Bitmap)
  (lambda [src [dest-density (default-bitmap-density)]]
    (cond [(fl= (real->double-flonum (send src get-backing-scale)) dest-density) src]
          [else (bitmap_alter_density (send src get-handle) dest-density)])))

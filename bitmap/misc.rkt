#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/digicore.rkt")
(require "digitama/unsafe/draw.rkt")
(require "digitama/unsafe/image.rkt")

(define bitmap-size : (case-> [Bitmap -> (Values Positive-Flonum Positive-Flonum)]
                              [Bitmap Nonnegative-Real -> (Values Nonnegative-Real Nonnegative-Real)]
                              [Bitmap Nonnegative-Real Nonnegative-Real -> (Values Nonnegative-Real Nonnegative-Real)])
  (case-lambda [(bmp) (bitmap-flsize bmp)]
               [(bmp ratio) (let ([% (real->double-flonum ratio)]) (bitmap-flsize* bmp % %))]
               [(bmp w% h%) (bitmap-flsize* bmp (real->double-flonum w%) (real->double-flonum h%))]))

(define bitmap-size+density : (Bitmap -> (Values Positive-Flonum Positive-Flonum Positive-Flonum))
  (lambda [bmp]
    (define-values (w h) (bitmap-flsize bmp))
    (values w h (bitmap-density bmp))))

(define bitmap-intrinsic-size : (-> Bitmap (Values Positive-Flonum Positive-Flonum))
  (lambda [bmp]
    (define density : Positive-Flonum (bitmap-density bmp))
    (bitmap-flsize* bmp density density)))

(define bitmap-alter-density : (->* (Bitmap) (Positive-Flonum) Bitmap)
  (lambda [src [dest-density (default-bitmap-density)]]
    (cond [(fl= (bitmap-density src) dest-density) src]
          [else (bitmap_alter_density (bitmap-surface src) dest-density)])))

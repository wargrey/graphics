#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [bitmap-intrinsic-flsize bitmap-intrinsic-size]))

(require "draw.rkt")
(require "digitama/unsafe/draw.rkt")
(require "digitama/unsafe/resize.rkt")

(define bitmap-size : (case-> [Bitmap -> (Values Positive-Flonum Positive-Flonum)]
                              [Bitmap Nonnegative-Real -> (Values Nonnegative-Flonum Nonnegative-Flonum)]
                              [Bitmap Nonnegative-Real Nonnegative-Real -> (Values Nonnegative-Flonum Nonnegative-Flonum)])
  (case-lambda [(bmp) (bitmap-flsize bmp)]
               [(bmp ratio) (let ([% (real->double-flonum ratio)]) (bitmap-flsize* bmp % %))]
               [(bmp w% h%) (bitmap-flsize* bmp (real->double-flonum w%) (real->double-flonum h%))]))

(define bitmap-size+density : (Bitmap -> (Values Positive-Flonum Positive-Flonum Positive-Flonum))
  (lambda [bmp]
    (define-values (w h) (bitmap-flsize bmp))
    (values w h (bitmap-density bmp))))

(define bitmap-alter-density : (->* (Bitmap) (Positive-Flonum) Bitmap)
  (lambda [src [dest-density (default-bitmap-density)]]
    (define density : Positive-Flonum (bitmap-density src))
    (cond [(fl= density dest-density) src]
          [else (let-values ([(flwidth flheight) (bitmap-flsize* src density density)])
                  (bitmap_section (bitmap-surface src) 0.0 0.0
                                  (fl/ flwidth dest-density)
                                  (fl/ flheight dest-density)
                                  dest-density))])))

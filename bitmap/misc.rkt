#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/base.rkt")
(require "digitama/unsafe/resize.rkt")
(require "digitama/unsafe/convert.rkt")

(define bitmap-alter-density : (->* (Bitmap) (Positive-Flonum) Bitmap)
  (lambda [src [dest-density (default-bitmap-density)]]
    (define density : Positive-Flonum (bitmap-density src))
    (cond [(= density dest-density) src]
          [else (let-values ([(flwidth flheight) (bitmap-flsize src density density)])
                  (bitmap_section (bitmap-surface src) 0.0 0.0
                                  (/ flwidth dest-density)
                                  (/ flheight dest-density)
                                  dest-density))])))

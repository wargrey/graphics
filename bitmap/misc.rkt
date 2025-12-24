#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/self.rkt")
(require "digitama/unsafe/adjust.rkt")

(require geofun/digitama/base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-alter-density : (->* (Bitmap) (Positive-Flonum) Bitmap)
  (lambda [src [dest-density (default-bitmap-density)]]
    (define density : Positive-Flonum (bitmap-density src))
    (cond [(= density dest-density) src]
          [else (let ([s (/ density dest-density)])
                  (bitmap_scale (bitmap-surface src)
                                s s dest-density))])))


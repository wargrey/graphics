#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out typed/racket/draw))

(require typed/racket/draw)

(require "digitama/digicore.rkt")
(require "constructor.rkt")

(define bitmap-dc : (->* ((-> (Instance Bitmap-DC%) Nonnegative-Flonum Nonnegative-Flonum Any) Nonnegative-Real)
                         ((Option Nonnegative-Real) Positive-Flonum)
                         Bitmap)
  (lambda [draw-with w [h #false] [density (default-bitmap-density)]]
    (define bmp : Bitmap (bitmap-blank w h #:density density))
    (define dc : (Instance Bitmap-DC%) (send bmp make-dc))
    (send dc set-smoothing 'aligned)
    ;;; let the device deal with non-integer size
    (draw-with dc (real->double-flonum w) (real->double-flonum (or h w)))
    bmp))

#lang typed/racket

(require bitmap/base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(for/list : (Listof Bitmap) ([angle (in-range 0.0 361.0 10.0)])
  (bitmap-cc-superimpose
   (bitmap-parallelogram 100 50 angle #:radian? #false #:stroke angle)
   (bitmap-text angle #:color angle)))

#lang typed/racket

(require bitmap/base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(for/list : (Listof Bitmap) ([angle (in-range 0.0 361.0 10.0)])
  (bitmap-frame
   (bitmap-cc-superimpose
    (bitmap-parallelogram 100 50 (degrees->radians angle) #:stroke angle)
    (bitmap-text angle #:color angle))))

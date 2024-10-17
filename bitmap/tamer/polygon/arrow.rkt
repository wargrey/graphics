#lang typed/racket

(require bitmap/base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(for/list : (Listof Bitmap) ([angle (in-range 0.0 361.0 10.0)])
  (bitmap-frame
   (bitmap-arrow 4 64 angle #:radian? #false #:fill angle #:stroke #false)))

(for/list : (Listof Bitmap) ([angle (in-range 0.0 361.0 10.0)])
  (bitmap-frame
   (bitmap-arrowhead 64 angle #:radian? #false #:stroke angle)))

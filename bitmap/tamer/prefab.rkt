#lang typed/racket

(module workaround typed/racket
  (require typed/racket/unsafe)
  (unsafe-provide bitmap-plain-frame)

  (require bitmap/base)
  (require bitmap/constants)

  (define (bitmap-plain-frame [bmp : Bitmap]) : Bitmap
    (bitmap-frame bmp #:border dot-dash-frame)))

(require bitmap/prefab)
(require (for-syntax "cairo/mesh.rkt"))
(require (for-syntax (submod "." workaround)))

(time (prefab-bitmap (time (bitmap-plain-frame (cairo-mesh-pattern 256.0 256.0 0.32)))))

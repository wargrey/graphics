#lang typed/racket

(require bitmap/prefab)
(require (for-syntax "cairo/mesh.rkt"))

(time (prefab-bitmap (time (cairo-mesh-pattern 256.0 256.0 0.32))))

(module workaround typed/racket/base
  (require typed/racket/unsafe)
  (unsafe-provide bitmap-plain-frame)

  (require bitmap/digitama/draw)
  (require bitmap/constructor)
  (require bitmap/constants)

  (define (bitmap-plain-frame [bmp : Bitmap]) : Bitmap
    (bitmap-frame bmp #:border dot-dash-frame)))

;;;
; This example is also work, however it may crash the parallel compiler:
;   Terminating app due to uncaught exception 'NSInternalInconsistencyException',
;   reason: 'nextEventMatchingMask should only be called from the Main Thread!
#;(require (for-syntax (submod "." workaround)))
#;(time (prefab-bitmap (time (bitmap-plain-frame (cairo-mesh-pattern 256.0 256.0 0.32)))))

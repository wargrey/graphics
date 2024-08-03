#lang typed/racket/base

(require typed/racket/unsafe)

(require bitmap)

(unsafe-require/typed
 pangocairo/tamer/mesh
 [cairo-mesh-pattern (-> Flonum Flonum Flonum Flonum Bitmap)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mesh-pattern (cairo-mesh-pattern 64.0 64.0 0.32 2.0))
(bitmap-cellophane mesh-pattern 1.00)
(bitmap-cellophane mesh-pattern 0.64)
(bitmap-cellophane mesh-pattern 0.32)
(bitmap-cellophane mesh-pattern 0.16)
(bitmap-cellophane mesh-pattern 0.00)

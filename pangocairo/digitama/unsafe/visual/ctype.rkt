#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module unsafe racket/base
  (provide (all-defined-out) phantom-bytes?)
  (provide (rename-out [cpointer? cairo-surface?]
                       [cpointer? bitmap-surface?]
                       [cpointer? abstract-surface?]
                       [cpointer? svg-surface?]
                       [cpointer? pdf-surface?]
                       [cpointer? cairo-dc?]))

  (require "../pangocairo.rkt")

  (define cairo-image-shadow-size
    (lambda [sfc]
      (make-phantom-bytes (unsafe-fx* (cairo_image_surface_get_stride sfc)
                                      (cairo_image_surface_get_height sfc))))))

(unsafe-require/typed/provide
 (submod "." unsafe)
 [#:opaque Phantom-Bytes phantom-bytes?]
 [#:opaque Cairo-Surface cairo-surface?]
 [#:opaque Abstract-Surface abstract-surface?]
 [#:opaque Bitmap-Surface bitmap-surface?]
 [#:opaque SVG-Surface svg-surface?]
 [#:opaque PDF-Surface pdf-surface?]
 [#:opaque Cairo-DC cairo-dc?]
 [cairo-image-shadow-size (-> Bitmap-Surface Phantom-Bytes)])

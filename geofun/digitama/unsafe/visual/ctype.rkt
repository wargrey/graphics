#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(module unsafe racket/base
  (provide (all-defined-out) phantom-bytes?)
  (provide (rename-out [cpointer? bitmap-surface?]
                       [cpointer? abstract-surface?]
                       [cpointer? svg-surface?]
                       [cpointer? pdf-surface?]
                       [cpointer? cairo-ctx?]))

  (require ffi/unsafe)
  (require racket/unsafe/ops)
  (require racket/draw/unsafe/cairo)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define cairo-image-shadow-size
    (lambda [sfc]
      (make-phantom-bytes (unsafe-fx* (cairo_image_surface_get_stride sfc)
                                      (cairo_image_surface_get_height sfc))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [#:opaque Phantom-Bytes phantom-bytes?]
 [#:opaque Abstract-Surface abstract-surface?]
 [#:opaque Bitmap-Surface bitmap-surface?]
 [#:opaque SVG-Surface svg-surface?]
 [#:opaque PDF-Surface pdf-surface?]
 [#:opaque Cairo-Ctx cairo-ctx?]
 [cairo-image-shadow-size (-> Bitmap-Surface Phantom-Bytes)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Cairo-Surface (U Bitmap-Surface Abstract-Surface))
(define-type Cairo-Stream-Surface (U PDF-Surface SVG-Surface))

(define-type (Gairo-Surface-Draw! Master)
  (-> Master Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Any))

(define-type (Cairo-Create-Surface SFC)
  (-> Nonnegative-Flonum Nonnegative-Flonum Positive-Flonum (Option Byte)
      (Values SFC Nonnegative-Flonum Nonnegative-Flonum)))

(define-type (Cairo-Create-Surface+Ctx SFC)
  (-> Nonnegative-Flonum Nonnegative-Flonum Positive-Flonum Boolean (Option Byte)
      (Values Abstract-Surface Cairo-Ctx Nonnegative-Flonum Nonnegative-Flonum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cairo-surface? : (-> Any Boolean : Cairo-Surface)
  (lambda [v]
    (or (bitmap-surface? v)
        (abstract-surface? v))))

(define cairo-stream-surface? : (-> Any Boolean : Cairo-Stream-Surface)
  (lambda [v]
    (or (pdf-surface? v)
        (svg-surface? v))))

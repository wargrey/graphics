#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)
(require geofun/digitama/unsafe/visual/ctype)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module unsafe racket/base
  (provide (all-defined-out))

  (require racket/unsafe/ops)
  (require racket/draw/unsafe/cairo)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (bitmap-surface-content-size sfc)
    (unsafe-fx* (cairo_image_surface_get_stride sfc)
                (cairo_image_surface_get_height sfc)))
  
  (define (bitmap-surface-intrinsic-size sfc)
    (values (cairo_image_surface_get_width sfc)
            (cairo_image_surface_get_height sfc)))

  (define (bitmap-surface-rendered-size sfc density)
    (define-values (width height) (bitmap-surface-intrinsic-size sfc))
    (values (unsafe-fl/ (unsafe-fx->fl width) density)
            (unsafe-fl/ (unsafe-fx->fl height) density)))

  (define (bitmap-surface-data sfc)
    (values (cairo_image_surface_get_data* sfc)
            (bitmap-surface-content-size sfc)))

  (define (bitmap-surface-metrics sfc components)
    (define-values (data total) (bitmap-surface-data sfc))
    (define stride (cairo_image_surface_get_stride sfc))
    (values data total stride
            (unsafe-fxquotient stride components)
            (unsafe-fxquotient total stride))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [bitmap-surface-data (-> Bitmap-Surface (Values Bytes Index))]
 [bitmap-surface-rendered-size (-> Bitmap-Surface Positive-Flonum (Values Nonnegative-Flonum Nonnegative-Flonum))])

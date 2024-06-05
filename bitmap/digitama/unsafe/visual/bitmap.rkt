#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

;;; NOTE
; The density should only affect the displaying of bitmap
; The size of a bitmap therefore always the same as the actual size of its `surface`

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module unsafe racket/base
  (provide (all-defined-out) phantom-bytes?)
  (provide (rename-out [cpointer? bitmap-surface?]))

  (require "../pangocairo.rkt")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (bitmap-surface-content-size sfc)
    (unsafe-fx* (cairo_image_surface_get_stride sfc)
                (cairo_image_surface_get_height sfc)))
  
  (define (bitmap-surface-intrinsic-size sfc)
    (values (cairo_image_surface_get_width sfc)
            (cairo_image_surface_get_height sfc)))

  (define (bitmap-surface-size sfc density)
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
            (unsafe-fxquotient total stride)))

  (define (bitmap-surface->stream-bytes sfc format name density)
    (define /dev/sfcout (open-output-bytes name))
    (bitmap-surface-save sfc /dev/sfcout format density)
    (get-output-bytes /dev/sfcout))

  (define (bitmap-surface-save bmp-sfc /dev/sfcout format density)
    (start-breakable-atomic)
    (case format
      [(svg) (bitmap-surface-save-with cairo_svg_surface_create_for_stream bmp-sfc /dev/sfcout density 4096)]
      [(pdf) (bitmap-surface-save-with cairo_pdf_surface_create_for_stream bmp-sfc /dev/sfcout density 256)]
      [else  (bitmap-surface-save-as-png bmp-sfc /dev/sfcout density 4096)])
    (end-breakable-atomic))

  (define (bitmap-surface-save-as-png bmp-sfc /dev/pngout density pool-size)
    (cairo_surface_write_to_png_stream bmp-sfc (make-cairo-image-surface-writer /dev/pngout pool-size)))

  (define (bitmap-surface-save-with create_for_stream bmp-sfc /dev/strout density pool-size)
    (define-values (width height) (bitmap-surface-size bmp-sfc density))
    (define vec-sfc (create_for_stream (make-cairo-vector-surface-writer /dev/strout pool-size) width height))
    (define vec-cr (cairo_create vec-sfc))

    (unless (unsafe-fl= density 1.0)
      (define s (unsafe-fl/ 1.0 density))
      (cairo_scale vec-cr s s))

    (cairo_set_source_surface vec-cr bmp-sfc 0.0 0.0)
    (cairo_paint vec-cr)
    (cairo_surface_flush vec-sfc)
    (cairo_surface_destroy vec-sfc)
    (cairo_destroy vec-cr)))

(unsafe-require/typed/provide
 (submod "." unsafe)
 [#:opaque Bitmap-Surface bitmap-surface?]
 [#:opaque Phantom-Bytes phantom-bytes?]
 [bitmap-surface-intrinsic-size (-> Bitmap-Surface (Values Positive-Index Positive-Index))]
 [bitmap-surface-data (-> Bitmap-Surface (Values Bytes Index))]
 [bitmap-surface->stream-bytes (-> Bitmap-Surface Symbol Symbol Flonum Bytes)]
 [bitmap-surface-save (-> Bitmap-Surface Output-Port Symbol Positive-Flonum Void)])

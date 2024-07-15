#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "ctype.rkt")

;;; NOTE
; The density should only affect the displaying of bitmap
; The size of a bitmap therefore always the same as the actual size of its `surface`

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module unsafe racket/base
  (provide (all-defined-out))

  (require "../pangocairo.rkt")
  
  (require "../stream/pdf.rkt")
  (require "../stream/svg.rkt")
  (require "../stream/png.rkt")

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
            (unsafe-fxquotient total stride)))

  (define (bitmap-surface->stream-bytes sfc format name density)
    (define /dev/sfcout (open-output-bytes name))
    (bitmap-surface-save sfc /dev/sfcout format density)
    (get-output-bytes /dev/sfcout))

  (define (bitmap-surface-save bmp-sfc /dev/sfcout format density)
    (case format
      [(svg) (bitmap-surface-save-with cairo-svg-stream-write bmp-sfc /dev/sfcout density)]
      [(pdf) (bitmap-surface-save-with cairo-pdf-stream-write bmp-sfc /dev/sfcout density)]
      [else  (cairo-png-stream-write /dev/sfcout (λ [] (values bmp-sfc #false)))]))

  (define (bitmap-surface-save-with stream-write bmp-sfc /dev/strout density)
    (define-values (width height) (bitmap-surface-rendered-size bmp-sfc density))

    (stream-write /dev/strout width height
                  (λ [vec-cr flwidth flheight]
                    (unless (unsafe-fl= density 1.0)
                      (define s (unsafe-fl/ 1.0 density))
                      (cairo_scale vec-cr s s))
                    
                    (cairo_set_source_surface vec-cr bmp-sfc 0.0 0.0)
                    (cairo_paint vec-cr)))))

(unsafe-require/typed/provide
 (submod "." unsafe)
 [bitmap-surface-data (-> Bitmap-Surface (Values Bytes Index))]
 [bitmap-surface->stream-bytes (-> Bitmap-Surface Symbol Symbol Flonum Bytes)]
 [bitmap-surface-save (-> Bitmap-Surface (U Output-Port Path-String) Symbol Positive-Flonum Void)])

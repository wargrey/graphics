#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "ctype.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module unsafe racket/base
  (provide (all-defined-out))
  (provide (rename-out [cairo_recording_surface_get_extents abstract-surface-extent]
                       [cairo_recording_surface_ink_extents abstract-surface-bbox]))
  
  (require "../pangocairo.rkt")
  
  (require "../stream/pdf.rkt")
  (require "../stream/svg.rkt")
  (require "../stream/png.rkt")

  (require (submod "../surface/abstract.rkt" unsafe))
  (require (submod "../surface/image.rkt" unsafe))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (abstract-surface-extent* sfc)
    (define-values (?lt w h) (cairo_recording_surface_get_extents sfc))

    (if (not ?lt)
        (cairo_recording_surface_ink_extents sfc)
        (values ?lt w h)))

  (define (abstract-surface->stream-bytes sfc format name density)
    (define /dev/sfcout (open-output-bytes name))
    (abstract-surface-save sfc /dev/sfcout format density)
    (get-output-bytes /dev/sfcout))

  (define (abstract-surface-save sfc /dev/sfcout format density)
    (case format
      [(svg) (abstract-surface-save-with cairo-svg-stream-write /dev/sfcout sfc)]
      [(pdf) (abstract-surface-save-with cairo-pdf-stream-write /dev/sfcout sfc)]
      [else  (cairo-png-stream-write /dev/sfcout
                                     (λ [] (let-values ([(png-sfc _w _h) (abstract-surface->image-surface sfc density)])
                                             (values png-sfc #true))))]))

  (define (abstract-surface-save-with stream-write /dev/strout abs-sfc)
    (define-values (pos width height) (abstract-surface-extent* abs-sfc))

    (stream-write /dev/strout width height
                  (λ [master cr x0 y0 flwidth flheight]
                    (cairo_set_source_surface cr abs-sfc
                                              (unsafe-fl- x0 (unsafe-flreal-part pos))
                                              (unsafe-fl- y0 (unsafe-flimag-part pos)))
                    (cairo_paint cr))
                  #false 0.0 0.0))

  (define (abstract-surface->image-surface abs-sfc density)
    (define-values (pos flwidth flheight) (abstract-surface-extent* abs-sfc))
    (define-values (bmp-sfc fxwidth fxheight) (cairo-create-image-surface flwidth flheight density))
    (define bmp-cr (cairo_create bmp-sfc))

    (unless (unsafe-fl= density 1.0)
      (cairo_scale bmp-cr density density))

    (cairo_set_source_surface bmp-cr abs-sfc
                              (unsafe-fl- 0.0 (unsafe-flreal-part pos))
                              (unsafe-fl- 0.0 (unsafe-flimag-part pos)))
    (cairo_paint bmp-cr)
    (cairo_destroy bmp-cr)

    (values bmp-sfc fxwidth fxheight))

  (define (create-abstract-surface-from-image-surface flwidth flheight img-sfc density)
    (define img-width  (unsafe-fx->fl (cairo_image_surface_get_width img-sfc)))
    (define img-height (unsafe-fx->fl (cairo_image_surface_get_height img-sfc)))
    (define-values (abs-sfc abs-width abs-height) (cairo-create-abstract-surface flwidth flheight density))
    (define abs-cr (cairo_create abs-sfc))

    (cairo_scale abs-cr (unsafe-fl/ abs-width img-width) (unsafe-fl/ abs-height img-height))
    (cairo_set_source_surface abs-cr img-sfc 0.0 0.0)
    (cairo_paint abs-cr)
    (cairo_destroy abs-cr)

    abs-sfc)

  (define (abstract-surface-stamp-onto-bitmap-surface bmp-sfc abs-sfc dx dy density)
    (define-values (pos flwidth flheight) (abstract-surface-extent* abs-sfc))
    (define bmp-cr (cairo_create bmp-sfc))

    (unless (unsafe-fl= density 1.0)
      (cairo_scale bmp-cr density density))
    
    (cairo_set_source_surface bmp-cr abs-sfc
                              (unsafe-fl- dx (unsafe-flreal-part pos))
                              (unsafe-fl- dy (unsafe-flimag-part pos)))
    (cairo_paint bmp-cr)
    (cairo_destroy bmp-cr)
    (void)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [abstract-surface-extent (-> Abstract-Surface (Values (Option Float-Complex) Nonnegative-Flonum Nonnegative-Flonum))]
 [abstract-surface-extent* (-> Abstract-Surface (Values Float-Complex Nonnegative-Flonum Nonnegative-Flonum))]
 [abstract-surface-bbox (-> Abstract-Surface (Values Float-Complex Nonnegative-Flonum Nonnegative-Flonum))]
 [abstract-surface->stream-bytes (-> Abstract-Surface Symbol Symbol Positive-Flonum Bytes)]
 [abstract-surface->image-surface (-> Abstract-Surface Positive-Flonum (Values Bitmap-Surface Positive-Index Positive-Index))]
 [abstract-surface-stamp-onto-bitmap-surface (-> Bitmap-Surface Abstract-Surface Flonum Flonum Positive-Flonum Void)]
 [abstract-surface-save (-> Abstract-Surface (U Output-Port Path-String) Symbol Positive-Flonum Void)]

 [create-abstract-surface-from-image-surface (-> Nonnegative-Flonum Nonnegative-Flonum Bitmap-Surface Positive-Flonum Abstract-Surface)])

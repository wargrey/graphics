#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "ctype.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module unsafe racket/base
  (provide (all-defined-out))
  (provide (rename-out [cairo_recording_surface_get_extents abstract-surface-extent]
                       [cairo_recording_surface_ink_extents abstract-surface-bbox]))

  (require racket/unsafe/ops)
  
  (require "../cairo.rkt")
  (require (submod "../surface/image.rkt" unsafe))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (abstract-surface-extent* sfc)
    (define-values (?lt w h) (cairo_recording_surface_get_extents sfc))

    (if (not ?lt)
        (cairo_recording_surface_ink_extents sfc)
        (values ?lt w h)))

  (define (abstract-surface->image-surface abs-sfc density filter)
    (define-values (pos flwidth flheight) (abstract-surface-extent* abs-sfc))
    (define-values (bmp-sfc fxwidth fxheight) (cairo-create-argb-image-surface flwidth flheight density filter))
    (define bmp-cr (cairo_create bmp-sfc))

    (unless (unsafe-fl= density 1.0)
      (cairo_scale bmp-cr density density))

    (cairo_set_source_surface bmp-cr abs-sfc
                              (unsafe-fl- 0.0 (unsafe-flreal-part pos))
                              (unsafe-fl- 0.0 (unsafe-flimag-part pos)))
    (cairo_paint bmp-cr)
    (values bmp-sfc fxwidth fxheight))

  (define (abstract-surface-stamp-onto-bitmap-surface bmp-sfc abs-sfc dx dy density)
    (define-values (pos flwidth flheight) (abstract-surface-extent* abs-sfc))
    (define bmp-cr (cairo_create bmp-sfc))

    (unless (unsafe-fl= density 1.0)
      (cairo_scale bmp-cr density density))
    
    (cairo_set_source_surface bmp-cr abs-sfc
                              (unsafe-fl- dx (unsafe-flreal-part pos))
                              (unsafe-fl- dy (unsafe-flimag-part pos)))
    (cairo_paint bmp-cr)
    (void)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [abstract-surface-extent (-> Abstract-Surface (Values (Option Float-Complex) Nonnegative-Flonum Nonnegative-Flonum))]
 [abstract-surface-extent* (-> Abstract-Surface (Values Float-Complex Nonnegative-Flonum Nonnegative-Flonum))]
 [abstract-surface-bbox (-> Abstract-Surface (Values Float-Complex Nonnegative-Flonum Nonnegative-Flonum))]
 [abstract-surface->image-surface (-> Abstract-Surface Positive-Flonum (Option Byte) (Values Bitmap-Surface Positive-Index Positive-Index))]
 [abstract-surface-stamp-onto-bitmap-surface (-> Bitmap-Surface Abstract-Surface Flonum Flonum Positive-Flonum Void)])

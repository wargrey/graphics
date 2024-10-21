#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/base)

(require "../cairo.rkt")
(require "../paint.rkt")
(require "../source.rkt")
(require "../visual/ctype.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dc_pattern : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Fill-Source Any)
  (lambda [cr x0 y0 width height background]
    (cairo-render-background cr background)))

(define dc_image : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Bitmap-Surface Any)
  (lambda [cr x0 y0 width height img-sfc]
    (define img-width  (exact->inexact (cairo_image_surface_get_width img-sfc)))
    (define img-height (exact->inexact (cairo_image_surface_get_height img-sfc)))

    (cairo_save cr)
    (cairo_scale cr (/ img-width width) (/ img-height height))
    (cairo_set_source_surface cr img-sfc x0 y0)
    (cairo_paint cr)
    (cairo_restore cr)))

(define dc_frame : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Bitmap-Surface
                       Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                       Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                       (Option Paint) (Option Fill-Source) Positive-Flonum Any)
  (lambda [cr x0 y0 width height src border-x border-y border-width border-height dest-x dest-y dest-width dest-height border background density]
    (cairo_rectangle cr (+ x0 border-x) (+ y0 border-y) border-width border-height)
    (cairo-render cr border background)
    (cairo-composite cr src (+ x0 dest-x) (+ y0 dest-y) dest-width dest-height density)))

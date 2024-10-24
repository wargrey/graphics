#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/base)

(require "../paint.rkt")
(require "../source.rkt")

(require "../typed/cairo.rkt")
(require "../visual/ctype.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dc_pattern : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Fill-Source Any)
  (lambda [cr x0 y0 width height background]
    (cairo-render-background cr background)))

(define dc_image : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Byte Bitmap-Surface Any)
  (lambda [cr x0 y0 width height filter img-sfc]
    (define img-width  (exact->inexact (cairo_image_surface_get_width  img-sfc)))
    (define img-height (exact->inexact (cairo_image_surface_get_height img-sfc)))

    (cairo-composite cr img-sfc x0 y0 width height filter
                     (/ width img-width) (/ height img-height))))

(define dc_frame : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Bitmap-Surface
                       Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                       Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                       (Option Paint) (Option Fill-Source) Byte Positive-Flonum Any)
  (lambda [cr x0 y0 width height src
              border-x border-y border-width border-height dest-x dest-y dest-width dest-height
              border background filter density]
    (define s (/ 1.0 density))
    
    (cairo_rectangle cr (+ x0 border-x) (+ y0 border-y) border-width border-height)
    (cairo-render cr border background)
    (cairo-composite cr src (+ x0 dest-x) (+ y0 dest-y) dest-width dest-height filter s s)))

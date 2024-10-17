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

(define dc_frame : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Bitmap-Surface
                       Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                       Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                       (Option Paint) (Option Fill-Source) Positive-Flonum Any)
  (lambda [cr x0 y0 width height src border-x border-y border-width border-height dest-x dest-y dest-width dest-height border background density]
    (cairo_rectangle cr border-x border-y border-width border-height)
    (cairo-render cr border background)
    (cairo-composite cr src dest-x dest-y dest-width dest-height density)))

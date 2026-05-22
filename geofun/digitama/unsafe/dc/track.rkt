#lang typed/racket/base

(provide (all-defined-out))

(require "path.rkt")

(require "../paint.rkt")
(require "../typed/cairo.rkt")

(require "../../geometry/footprint.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dc_track : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                       Geo-Path-Prints (Option Pen) (Option Brush) (U Pen (Listof Pen) False) Boolean
                       Any)
  (lambda [cr x0 y0 width height footprints stroke brush halo-strokes round?]
    (cairo_path cr footprints x0 y0 #false)

    (and halo-strokes
         (for ([halo-stroke (if (list? halo-strokes) (in-list halo-strokes) (in-value halo-strokes))])
           (cairo-set-stroke cr halo-stroke #false 1.0 round?)
           (cairo_stroke_preserve cr)))

    (cairo-render cr stroke brush)))

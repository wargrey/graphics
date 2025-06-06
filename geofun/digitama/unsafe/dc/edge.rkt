#lang typed/racket/base

(provide (all-defined-out))

(require "path.rkt")

(require "../source.rkt")
(require "../paint.rkt")
(require "../typed/cairo.rkt")

(require "../../paint/self.rkt")
(require "../../geometry/footprint.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dc_edge : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                      Geo-Path-Clean-Prints Float-Complex (Option Stroke)
                      Geo-Path-Prints (Option Stroke) (Option Fill-Source)
                      Geo-Path-Prints (Option Stroke) (Option Fill-Source)
                      (Pairof (Option Float-Complex) (Option Float-Complex))
                      Any)
  (lambda [cr x0 y0 flwidth flheight footprints offset stroke
              src-mkr-prints src-mkr-pen src-mkr-brush
              tgt-mkr-prints tgt-mkr-pen tgt-mkr-brush
              adjust-offset]
    (define-values (dx dy) (values (real-part offset) (imag-part offset)))
    
    (cairo_new_path cr)
    (cairo_clean_path cr footprints (+ x0 dx) (+ y0 dy) (car adjust-offset) (cdr adjust-offset))
    (cairo-render cr stroke)
    (dc-edge-draw-shape cr src-mkr-prints src-mkr-pen src-mkr-brush)
    (dc-edge-draw-shape cr tgt-mkr-prints tgt-mkr-pen tgt-mkr-brush)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dc-edge-draw-shape : (-> Cairo-Ctx Geo-Path-Prints (Option Stroke) (Option Fill-Source) Void)
  (lambda [cr prints stroke brush]
    (when (pair? prints)
      (cairo_new_path cr)
      (cairo_path cr prints 0.0 0.0)
      (cairo-render cr stroke brush))))

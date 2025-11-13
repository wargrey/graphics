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
                      Geo-Path-Clean-Prints Float-Complex (Option Pen)
                      Geo-Path-Prints (Option Pen) (Option Fill-Source) Float-Complex
                      Geo-Path-Prints (Option Pen) (Option Fill-Source) Float-Complex
                      (Pairof (Option Float-Complex) (Option Float-Complex))
                      Any)
  (lambda [cr x0 y0 flwidth flheight footprints offset stroke
              src-mkr-prints src-mkr-pen src-mkr-brush src-pos
              tgt-mkr-prints tgt-mkr-pen tgt-mkr-brush tgt-pos
              adjust-offset]
    (define-values (dx dy) (values (real-part offset) (imag-part offset)))
    
    (cairo_new_path cr)
    (cairo_clean_path cr footprints (+ x0 dx) (+ y0 dy) (car adjust-offset) (cdr adjust-offset) #false)
    (cairo-render cr stroke)

    ; `cairo_clear_path` above has already translated by `(dx, dy)`
    (dc-edge-draw-shape cr src-mkr-prints src-mkr-pen src-mkr-brush src-pos)
    (dc-edge-draw-shape cr tgt-mkr-prints tgt-mkr-pen tgt-mkr-brush tgt-pos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dc-edge-draw-shape : (-> Cairo-Ctx Geo-Path-Prints (Option Pen) (Option Fill-Source) Float-Complex Void)
  (lambda [cr prints stroke brush position]
    (when (pair? prints)
      (define px (real-part position))
      (define py (imag-part position))
      
      (cairo_new_path cr)
      (cairo_path cr prints px py #false)
      (cairo_translate cr (- px) (- py))
      (cairo-render cr stroke brush))))

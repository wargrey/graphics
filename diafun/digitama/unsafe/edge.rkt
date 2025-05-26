#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/unsafe/typed/cairo)
(require geofun/digitama/unsafe/dc/path)
(require geofun/digitama/unsafe/source)
(require geofun/digitama/unsafe/paint)

(require geofun/digitama/paint/self)
(require geofun/digitama/geometry/footprint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Edge-Marker (Immutable-Vector Geo-Path-Prints (Option Stroke) (Option Fill-Source)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dc_edge : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                      Geo-Path-Clean-Prints Float-Complex (Option Stroke)
                      Geo-Edge-Marker Geo-Edge-Marker (Pairof (Option Float-Complex) (Option Float-Complex))
                      Any)
  (lambda [cr x0 y0 flwidth flheight footprints offset stroke source target adjust-offset]
    (define-values (dx dy) (values (real-part offset) (imag-part offset)))
    
    (cairo_new_path cr)
    (cairo_clean_path cr footprints (+ x0 dx) (+ y0 dy) (car adjust-offset) (cdr adjust-offset))
    (cairo-render cr stroke)
    (dc-edge-draw-shape cr source)
    (dc-edge-draw-shape cr target)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dc-edge-draw-shape : (-> Cairo-Ctx Geo-Edge-Marker Void)
  (lambda [cr marker]
    (define prints : Geo-Path-Prints (vector-ref marker 0))
    
    (when (pair? prints)
      (cairo_new_path cr)
      (cairo_path cr prints 0.0 0.0)
      (cairo-render cr (vector-ref marker 1) (vector-ref marker 2)))))

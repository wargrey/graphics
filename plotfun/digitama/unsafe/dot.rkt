#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)

(require geofun/digitama/paint/self)
(require geofun/digitama/geometry/footprint)

(require geofun/digitama/unsafe/typed/cairo)
(require geofun/digitama/unsafe/typed/more)

(require geofun/digitama/unsafe/paint)
(require geofun/digitama/unsafe/source)
(require geofun/digitama/unsafe/dc/path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dc_line : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum (Listof Float-Complex) Pen Boolean Any)
  (lambda [cr x0 y0 flwidth flheight vertices stroke close?]
    (dc_polyline cr x0 y0 flwidth flheight vertices stroke close?)))

(define dc_scatter : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                         Float-Complex (Listof Float-Complex) Geo-Path-Prints
                         (Option Pen) (Option Brush) Boolean Any)
  (lambda [cr x y flwidth flheight origin vertices marker pen brush clip?]
    (define x0 (real-part origin))
    (define y0 (imag-part origin))
    
    (when (or clip?)
      (cairo-clip cr x y flwidth flheight))

    (cairo_translate cr (- x x0) (- y y0))
    
    (for ([pt (in-list vertices)])
      (define px (real-part pt))
      (define py (imag-part pt))

      (cairo_new_path cr)
      (cairo_path cr marker px py #false)
      (cairo_translate cr (- px) (- py))
      
      (cairo-render cr pen brush))))

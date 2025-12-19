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

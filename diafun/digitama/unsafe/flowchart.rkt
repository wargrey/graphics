#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

(require geofun/digitama/unsafe/paint)
(require geofun/digitama/unsafe/typed/cairo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dc_general_storage : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum (Option Pen) (Option Brush) Any)
  (lambda [cr x0 y0 flwidth flheight aradius stroke background]
    (define bradius (* flheight 0.5))
    (define cy (+ y0 bradius))
    
    (cairo_new_path cr)
    (cairo-negative-arc cr (+ x0 aradius) cy aradius bradius -pi/2  pi/2)
    (cairo-positive-arc cr (+ x0 flwidth) cy aradius bradius  pi/2 3pi/2)
    (cairo_close_path cr)
    (cairo-render cr stroke background)))

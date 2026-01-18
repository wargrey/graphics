#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

(require geofun/font)
(require geofun/color)
(require geofun/stroke)

(require geofun/digitama/paint/self)
(require geofun/digitama/path/tip/arrow)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-preset-axis-font : Font (desc-font #:size 12.0))
(define plot-preset-axis-label-font : Font (desc-font #:family 'math #:size 16.0))
(define plot-preset-axis-digit-font : Font (desc-font #:family 'monospace #:size 12.0))
(define plot-preset-axis-pen : Pen (desc-stroke #:color (rgb 0.2 0.2 0.2 1.0) #:width 1.5))
(define plot-preset-axis-arrow : Geo:Tip:Arrow (make-geo:tip:arrow #:radius (~% 256)))

(define plot-preset-major-grid-pen : Pen (desc-stroke #:color (rgb 0.70 0.70 0.70 1.0) #:width 1.0 #:cap 'round))
(define plot-preset-minor-grid-pen : Pen (desc-stroke #:color (rgb 0.85 0.85 0.85 1.0) #:width 0.6 #:cap 'butt))
(define plot-preset-aaline-pen : Pen (desc-stroke #:color (rgb 0.6 0.6 0.8 0.8) #:width 1.0 #:cap 'round #:dash #(3.0 2.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-preset-marker-font : Font (desc-font #:family 'math #:size 12.0))
(define plot-preset-marker-pin-pen : Pen (desc-stroke #:color 'grey #:width 1.0))

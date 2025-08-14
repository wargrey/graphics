#lang typed/racket/base

(provide (all-defined-out))

(require geofun/font)
(require geofun/color)
(require geofun/stroke)

(require geofun/digitama/paint/self)
(require geofun/digitama/path/tip/arrow)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-axis-font : Font (desc-font #:size 12.0))
(define default-axis-label-font : Font (desc-font #:family 'math #:size 16.0))
(define default-axis-digit-font : Font (desc-font #:family 'monospace #:size 12.0))
(define default-axis-stroke : Stroke (desc-stroke #:color (rgb 0.2 0.2 0.2 1.0) #:width 1.5))
(define default-axis-arrow : Geo:Tip:Arrow (make-geo:tip:arrow #:radius '(256 %)))

(define default-major-grid-stroke : Stroke (desc-stroke #:color (rgb 0.70 0.70 0.70 1.0) #:width 1.0))
(define default-minor-grid-stroke : Stroke (desc-stroke #:color (rgb 0.85 0.85 0.85 1.0) #:width 0.6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-marker-font : Font (desc-font #:family 'math #:size 12.0))
(define default-marker-pin-stroke : Stroke (desc-stroke #:color gray #:width 1.0))

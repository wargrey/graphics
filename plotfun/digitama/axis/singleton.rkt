#lang typed/racket/base

(provide (all-defined-out))

(require geofun/color)
(require geofun/font)

(require geofun/digitama/edge/tip/arrow)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-axis-font : Font (desc-font #:size 12.0))
(define default-axis-label-font : Font (desc-font #:family 'math #:size 16.0))
(define default-axis-digit-font : Font (desc-font #:family 'monospace #:size 12.0))

(define default-axis-color : FlRGBA (rgb 0.2 0.2 0.2 1.0))
(define default-axis-arrow : Geo:Tip:Arrow (make-geo:tip:arrow #:radius -2.5))

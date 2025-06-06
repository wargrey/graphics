#lang typed/racket/base

(provide (all-defined-out))

(require geofun/color)
(require geofun/font)

(require geofun/digitama/edge/marker/arrow)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-digit-font : Font (desc-font #:family 'monospace #:size 12.0))
(define default-label-font : Font (desc-font #:family 'math #:size 12.0))

(define default-axis-color : FlRGBA (rgb* 'DarkSlateGray))
(define default-axis-arrow : Geo:Mkr:Arrow (make-geo:mkr:arrow #:radius -2.5))

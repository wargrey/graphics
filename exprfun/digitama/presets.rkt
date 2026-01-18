#lang typed/racket/base

(provide (all-defined-out))

(require geofun/font)
(require geofun/stroke)
(require geofun/digitama/paint/self)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define expr-preset-expr-font : Font (desc-font #:family 'monospace))
(define expr-preset-header-font : Font (desc-font #:size 'large #:weight 'bold))

(define expr-preset-slot-stroke : Pen (desc-stroke #:width 1.0 #:color 'Silver))

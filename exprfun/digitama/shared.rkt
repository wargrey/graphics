#lang typed/racket/base

(provide (all-defined-out))

(require geofun/font)
(require geofun/stroke)
(require geofun/digitama/paint/self)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-expr-font : Font (desc-font #:family 'monospace))
(define default-header-font : Font (desc-font #:size 'large #:weight 'bold))

(define default-slot-stroke : Pen (desc-stroke #:width 1.0 #:color 'Silver))

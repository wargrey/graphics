#lang typed/racket/base

(provide (all-defined-out))

(require geofun/font)
(require geofun/color)
(require geofun/stroke)
(require geofun/digitama/paint/self)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-track-label-font : Font (desc-font #:size 'normal #:family 'monospace))
(define default-label-tag-font : Font (desc-font #:size 'normal #:family 'monospace))
(define default-block-brief-font : Font (desc-font #:size 'xx-large))
(define default-number-font : Font (desc-font #:family 'monospace))
(define default-table-header-font : Font (desc-font #:size 'large #:weight 'bold))

(define default-track-stroke : Pen (desc-stroke #:width 2.0 #:color 'DimGrey #:join 'round #:cap 'round))
(define default-block-stroke : Pen (desc-stroke #:width 2.0 #:color 'DarkGrey))
(define default-cell-stroke : Pen (desc-stroke #:width 1.0 #:color 'Silver))

(define default-black-stroke : Pen (desc-stroke #:width 2.0 #:color black))

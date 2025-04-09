#lang typed/racket/base

(provide (all-defined-out))

(require geofun/font)
(require geofun/stroke)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-edge-label-font : Font (desc-font #:size 'normal #:family 'monospace))
(define default-label-tag-font : Font (desc-font #:size 'normal #:family 'monospace))
(define default-node-label-font : Font (desc-font #:size 'xx-large))
(define default-number-font : Font (desc-font #:family 'monospace))
(define default-table-header-font : Font (desc-font #:size 'large #:weight 'bold))

(define default-edge-stroke : Stroke (desc-stroke #:width 2.0 #:color 'DimGrey #:join 'round #:cap 'round))
(define default-node-stroke : Stroke (desc-stroke #:width 2.0 #:color 'DarkGrey))
(define default-black-stroke : Stroke (desc-stroke #:width 2.0 #:color 'Black))

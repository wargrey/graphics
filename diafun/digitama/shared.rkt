#lang typed/racket/base

(provide (all-defined-out))

(require geofun/font)
(require geofun/stroke)

(require "edge/tip.rkt")
(require "edge/arrow.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-edge-label-font : Font (desc-font #:size 'small #:family 'monospace))
(define default-node-label-font : Font (desc-font #:size 'xx-large))
(define default-arrow-tip : Dia-Edge-Tip-Shape (make-dia-arrow-tip))

(define default-edge-stroke : Stroke (desc-stroke #:width 2.0 #:color 'DimGray #:join 'round #:cap 'butt))
(define default-node-stroke : Stroke (desc-stroke #:width 2.0 #:color 'DarkGray))

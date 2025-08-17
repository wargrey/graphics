#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/paint/self)
(require geofun/stroke)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-function-stroke : Stroke (desc-stroke #:width 1.5 #:join 'round #:cap 'round #:opacity 0.75))

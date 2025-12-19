#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/paint/self)
(require geofun/stroke)
(require geofun/fill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-visualizer-stroke : Pen (desc-stroke #:width 1.5 #:join 'round #:cap 'round #:opacity 0.75))
(define default-visualizer-brush : Brush (desc-brush #:opacity 0.618))

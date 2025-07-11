#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/base)

(require geofun/digitama/paint/self)
(require geofun/stroke)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define current-visualizer-color-pool : (Parameterof (Option (HashTable Natural FlRGBA))) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-function-stroke : Stroke (desc-stroke #:width 1.5 #:join 'round #:cap 'round #:opacity 0.75))

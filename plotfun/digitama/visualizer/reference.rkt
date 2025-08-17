#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/base)

(require "self.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define current-visualizer-color-pool : (Parameterof (Option (HashTable Symbol FlRGBA))) (make-parameter #false))
(define current-visualizer-polyline-pool : (Parameterof (Option (HashTable Symbol Plot-Visualizer))) (make-parameter #false))

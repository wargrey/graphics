#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define current-visualizer-color-pool : (Parameterof (Option (HashTable Natural FlRGBA))) (make-parameter #false))

#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)
(require geofun/font)

(require "../presets.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-cls-stereotype-gapsize : (Parameterof Length+%) (make-parameter 4.0))
(define default-cls-stereotype-font : (Parameterof (Option Font)) (make-parameter dia-preset-block-tag-font))

#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)
(require geofun/font)

(require "../presets.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-act-stereotype-gapsize : (Parameterof Length+%) (make-parameter 4.0))
(define default-act-stereotype-font : (Parameterof (Option Font)) (make-parameter dia-preset-block-tag-font))

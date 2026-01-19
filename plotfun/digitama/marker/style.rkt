#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)
(require digimon/struct)

(require geofun/color)
(require geofun/font)

(require geofun/digitama/paint/self)

(require "anchor.rkt")
(require "../presets.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct/parameter plot-mark-style : Plot-Mark-Style
  ([font : (Option Font) #false]
   [color : (Option Color) #false]
   [pin-pen : (Option Pen) plot-preset-marker-pin-pen]
   [pin-length : Length+% (&% 100)]
   [pin-angle : Real +nan.0]
   [gap-length : Length+% 0.0]
   [gap-angle : Real +nan.0]
   [anchor : Plot-Mark-Auto-Anchor plot-mark-auto-anchor]))

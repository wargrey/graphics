#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require geofun/digitama/paint/self)

(require "../../presets.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct/parameter plot-grid-style : Plot-Grid-Style
  ([major-pen : (Option Pen) plot-preset-major-grid-pen]
   [minor-pen : (Option Pen) plot-preset-minor-grid-pen]
   [minor-count :  (U False Index (-> Real Real Index)) 9]))

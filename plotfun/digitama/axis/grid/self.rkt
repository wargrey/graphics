#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)

(require geofun/digitama/paint/self)

(require "../singleton.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct/parameter plot-grid-style : Plot-Grid-Style
  ([major-stroke : (Option Stroke) default-major-grid-stroke]
   [minor-stroke : (Option Stroke) default-minor-grid-stroke]
   [minor-count : (Option Index) 9]))

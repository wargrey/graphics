#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require geofun/digitama/paint/self)

(require "../singleton.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct/parameter plot-grid-style : Plot-Grid-Style
  ([major-stroke : (Option Pen) default-major-grid-stroke]
   [minor-stroke : (Option Pen) default-minor-grid-stroke]
   [minor-count :  (U False Index (-> Real Real Index)) 9]))

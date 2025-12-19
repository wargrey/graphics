#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require geofun/digitama/paint/self)

(require "../../singleton.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct/parameter plot-grid-style : Plot-Grid-Style
  ([major-pen : (Option Pen) default-major-grid-pen]
   [minor-pen : (Option Pen) default-minor-grid-pen]
   [minor-count :  (U False Index (-> Real Real Index)) 9]))

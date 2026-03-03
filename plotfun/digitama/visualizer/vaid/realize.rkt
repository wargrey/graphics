#lang typed/racket/base

(provide (all-defined-out))

(require geofun/color)
(require geofun/digitama/path/dc)
(require geofun/digitama/paint/self)

(require "self.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-aaline-realize : (-> Plot-AALine Plot-Visual-Aid-Style (Listof Geo:Path))
  (case-lambda
    [(self master)
     null]))

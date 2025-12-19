#lang typed/racket/base

(provide (all-defined-out))

(require geofun/color)
(require geofun/digitama/paint/self)
(require geofun/digitama/dc/path)

(require "self.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-aaline-realize : (-> Plot-AALine Plot-Visual-Aid-Style (Listof Geo:Path))
  (case-lambda
    [(self master)
     null]))

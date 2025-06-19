#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)

(require geofun/color)
(require geofun/font)

(require geofun/digitama/layer/type)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct/parameter plot-marker-style : Plot-Marker-Style
  ([font : (Option Font) #false]
   [color : (Option Color) #false]
   [position : Flonum 1.0]
   [anchor : Geo-Pin-Anchor 'cb]
   [dot-radius : Real -1.2]))

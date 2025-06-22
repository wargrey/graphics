#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)

(require geofun/color)
(require geofun/font)

(require geofun/digitama/layer/type)
(require geofun/digitama/paint/self)

(require "../axis/singleton.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Plot-Marker-Length-Unit (U Flonum 'em))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct/parameter plot-marker-style : Plot-Marker-Style
  ([font : (Option Font) #false]
   [color : (Option Color) #false]
   [length-unit : Plot-Marker-Length-Unit 'em]
   [gap-length : Flonum 1.0]
   [pin-length : Flonum 1.0]
   [pin-stroke : (Option Stroke) default-marker-pin-stroke]
   [anchor : Geo-Pin-Anchor 'cb]))

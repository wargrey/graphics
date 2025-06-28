#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)
(require digimon/struct)

(require geofun/color)
(require geofun/font)

(require geofun/digitama/layer/type)
(require geofun/digitama/paint/self)

(require "../axis/singleton.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct/parameter plot-mark-style : Plot-Mark-Style
  ([font : (Option Font) #false]
   [color : (Option Color) #false]
   [pin-stroke : (Option Stroke) default-marker-pin-stroke]
   [pin-length : Real+% '(100 %)]
   [pin-angle : Real +nan.0]
   [gap-length : Real+% 0.0]
   [gap-angle : Real +nan.0]
   [anchor : Geo-Pin-Anchor 'cb]))

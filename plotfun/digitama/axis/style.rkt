#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)

(require geofun/color)
(require geofun/font)

(require geofun/digitama/layer/type)
(require geofun/digitama/edge/tip/self)

(require "singleton.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Plot-Axis-Label-Position (U 'axis 'digit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct/parameter plot-axis-style : Plot-Axis-Style
  ([thickness : Nonnegative-Flonum 1.5]
   [color : Color default-axis-color]
   [font : Font default-axis-font]
   [tick-thickness : Nonnegative-Flonum 1.0]
   [tick-length : Complex -3.0]
   [tick-color : (Option Color) #false]
   [tick-anchor : (U Geo-Pin-Anchor (Pairof Geo-Pin-Anchor Geo-Pin-Anchor)) (cons 'cb 'lc)]
   [digit-color : (Option Color) #false]
   [digit-font : (Option Font) default-axis-digit-font]
   [digit-position : Complex -1.2-0.618i]
   [label-font : (Option Font) default-axis-label-font]
   [label-color : (Option Color) #false]
   [label-position : Plot-Axis-Label-Position 'axis]
   [desc-font : (Option Font) #false]
   [desc-color : (Option Color) #false]))

(define-struct/parameter plot-axis-tip-style : Plot-Axis-Tip-Style
  ([positive-shape : (Option Geo-Tip) default-axis-arrow]
   [negative-shape : (Option Geo-Tip) #false]
   [margin : Complex 0.0-0.08i]))

(define-struct/parameter plot-mark-style : Plot-Mark-Style
  ([font : (Option Font) #false]
   [color : (Option Color) #false]
   [position : Flonum 1.0]
   [anchor : Geo-Pin-Anchor 'cb]
   [dot-radius : Real -1.2]))

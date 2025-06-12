#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)

(require geofun/color)
(require geofun/font)

(require geofun/digitama/layer/type)
(require geofun/digitama/edge/tip/self)

(require "singleton.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration plot-axis-style : Plot-Axis-Style
  #:format "default-plot-axis-~a"
  ([thickness : Nonnegative-Flonum 1.5]
   [color : Color default-axis-color]
   [label-font : (Option Font) default-label-font]
   [label-color : (Option Color) #false]
   [tick-thickness : Nonnegative-Flonum 1.0]
   [tick-length : Complex -4.0]
   [tick-color : (Option Color) #false]
   [tick-anchor : (U Geo-Pin-Anchor (Pairof Geo-Pin-Anchor Geo-Pin-Anchor)) 'cc]
   [digit-color : (Option Color) #false]
   [digit-font : Font default-digit-font]
   [digit-position : Complex -0.618]))

(define-configuration plot-axis-tip-style : Plot-Axis-Tip-Style
  #:format "default-plot-axis-tip-~a"
  ([positive-shape : (Option Geo-Tip) default-axis-arrow]
   [negative-shape : (Option Geo-Tip) #false]
   [margin : Complex 0.0-0.08i]))

(define-configuration plot-axis-real-style : Plot-Axis-Real-Style
  #:format "default-plot-axis-real-~a"
  ([font : (Option Font) #false]
   [color : (Option Color) #false]
   [position : Flonum 1.0]
   [anchor : Geo-Pin-Anchor 'cb]
   [dot-radius : Real -1.2]))

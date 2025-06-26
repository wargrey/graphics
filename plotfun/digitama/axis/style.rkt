#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require digimon/metrics)

(require geofun/color)
(require geofun/font)

(require geofun/digitama/layer/type)
(require geofun/digitama/paint/self)
(require geofun/digitama/edge/tip/self)

(require "singleton.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Plot-Axis-Tick-Placement (U 'positive 'center 'negative))
(define-type Plot-Axis-Label-Placement (U 'axis 'digit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct/parameter plot-axis-tip-style : Plot-Axis-Tip-Style
  ([positive-shape : (Option Geo-Tip) default-axis-arrow]
   [negative-shape : (Option Geo-Tip) #false]
   [positive-margin : Real+% '(8 %)]
   [negative-margin : Real+% 0.0]))

(define-struct/parameter plot-axis-style : Plot-Axis-Style
  ([stroke : Stroke default-axis-stroke]
   [font : Font default-axis-font]
   [tick-thickness : Real+% 1.0]
   [tick-length : Real+% '(300 %)]
   [tick-color : (Option Color) #false]
   [tick-placement : Plot-Axis-Tick-Placement 'positive]
   [digit-color : (Option Color) #false]
   [digit-font : (Option Font) default-axis-digit-font]
   [digit-position : Complex -1.2-0.618i]
   [label-font : (Option Font) default-axis-label-font]
   [label-color : (Option Color) #false]
   [label-placement : Plot-Axis-Label-Placement 'axis]
   [desc-font : (Option Font) #false]
   [desc-color : (Option Color) #false]
   [tip : Plot-Axis-Tip-Style (default-plot-axis-tip-style)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-no-tip
  (make-plot-axis-tip-style #:positive-shape #false #:negative-shape #false
                            #:positive-margin 0.0 #:negative-margin 0.0))

(define plot-bi-tip
  (make-plot-axis-tip-style #:negative-shape default-axis-arrow
                            #:positive-margin '(10 %) #:negative-margin '(10 %)))

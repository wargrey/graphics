#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require digimon/measure)

(require geofun/color)
(require geofun/font)

(require geofun/digitama/paint/self)
(require geofun/digitama/path/tick)
(require geofun/digitama/path/tip/self)

(require "../presets.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Plot-Axis-Label-Placement (U 'axis 'digit 'mirror 'digit-mirror 'mirror-digit))

(define plot-axis-style-xtick-placement : (-> Plot-Axis-Style Boolean Geo-Tick-Placement)
  (lambda [this screen?]
    (define placement (plot-axis-style-tick-placement this))

    (cond [(not screen?) placement]
          [(eq? placement 'positive) 'negative]
          [(eq? placement 'negative) 'positive]
          [else placement])))

(define plot-axis-style-xlabel-placement : (-> Plot-Axis-Style Boolean Plot-Axis-Label-Placement)
  (lambda [this screen?]
    (define placement (plot-axis-style-label-placement this))

    (cond [(not screen?) placement]
          [(eq? placement 'digit) 'mirror]
          [(eq? placement 'mirror) 'digit]
          [(eq? placement 'digit-mirror) 'mirror]
          [(eq? placement 'mirror-digit) 'digit]
          [else placement])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct/parameter plot-axis-tip-style : Plot-Axis-Tip-Style
  ([positive-shape : (Option Geo-Tip) plot-preset-axis-arrow]
   [negative-shape : (Option Geo-Tip) #false]
   [positive-margin : Length+% (&% 8)]
   [negative-margin : Length+% 0.0]))

(define-struct/parameter plot-axis-style : Plot-Axis-Style
  ([pen : Pen plot-preset-axis-pen]
   [font : Font plot-preset-axis-font]
   [tick-thickness : Real+% 1.0]
   [tick-length : Length+% (&% 300)]
   [tick-color : (Option Color) #false]
   [tick-placement : Geo-Tick-Placement 'positive]
   [minor-tick-length : Length+% (&% 61.8)]
   [digit-color : (Option Color) #false]
   [digit-font : (Option Font) plot-preset-axis-digit-font]
   [digit-position : Complex -1.2-0.618i]
   [label-font : (Option Font) plot-preset-axis-label-font]
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
  (make-plot-axis-tip-style #:negative-shape plot-preset-axis-arrow
                            #:positive-margin (&% 10) #:negative-margin (&% 10)))

#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require geofun/digitama/paint/self)
(require geofun/digitama/path/tip/self)

(require "../../singleton.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Plot-AALine-Visible-Axis (U 'x 'y 'both))

(define-type Plot-Visual-Aid-Line (U Plot-AALine))
(define-type Plot-Visual-Aid (U Plot-Visual-Aid-Line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct plot-aaline : Plot-AALine
  ([point : Complex]
   [pen : (Option Pen) #false]
   [x-tip : (U Void False Geo-Tip) (void)]
   [y-tip : (U Void False Geo-Tip) (void)]
   [axes : Plot-AALine-Visible-Axis 'both]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct/parameter plot-visual-aid-style : Plot-Visual-Aid-Style
  ([aaline-pen : Pen default-aaline-pen]))

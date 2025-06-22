#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require geofun/font)
(require geofun/color)

(require geofun/digitama/layer/type)
(require geofun/digitama/paint/self)

(require "../axis/style.rkt")
(require "style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-marker-style-values : (->* ((Option Plot-Marker-Style) Plot-Axis-Style)
                                        ((Option Stroke))
                                        (Values Stroke Font Color Plot-Marker-Length-Unit Flonum Flonum Geo-Pin-Anchor))
  (lambda [self master [pen #false]]
    (if (or self)
        (values (or (plot-marker-style-pin-stroke self)
                    pen (plot-axis-style-stroke master))
                (or (plot-marker-style-font self)
                    (plot-axis-style-font master))
                (or (plot-marker-style-color self)
                    (stroke-color (or pen (plot-axis-style-stroke master))))
                (plot-marker-style-length-unit self)
                (plot-marker-style-gap-length self)
                (plot-marker-style-pin-length self)
                (plot-marker-style-anchor self))
        (plot-marker-style-values (default-plot-marker-style) master pen))))

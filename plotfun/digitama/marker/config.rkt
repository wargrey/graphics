#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require geofun/font)
(require geofun/color)

(require geofun/digitama/layer/type)
(require geofun/digitama/paint/self)

(require "style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-mark-style-values : (-> (Option Plot-Mark-Style) Font Stroke
                                     Nonnegative-Flonum Real Real
                                     (Values Stroke Font Color Float-Complex Float-Complex Geo-Pin-Anchor))
  (lambda [self fallback-font fallback-pen 100% pin-angle gap-angle]
    (if (or self)
        (values (or (plot-mark-style-pin-stroke self) fallback-pen)
                (or (plot-mark-style-font self) fallback-font)
                (or (plot-mark-style-color self) (stroke-color fallback-pen))
                (make-polar (~length (plot-mark-style-pin-length self) 100%)
                            (let ([a (plot-mark-style-pin-angle self)])
                              (real->double-flonum (if (rational? a) a pin-angle))))
                (make-polar (~length (plot-mark-style-gap-length self) 100%)
                            (let ([a (plot-mark-style-gap-angle self)])
                              (real->double-flonum (if (rational? a) a gap-angle))))
                (plot-mark-style-anchor self))
        (plot-mark-style-values (default-plot-mark-style)
                                fallback-font fallback-pen
                                100% pin-angle gap-angle))))

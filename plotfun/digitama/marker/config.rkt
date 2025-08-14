#lang typed/racket/base

(provide (all-defined-out))

(require geofun/font)
(require geofun/color)

(require geofun/digitama/paint/self)

(require "style.rkt")
(require "guard.rkt")
(require "anchor.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-mark-visual-values : (case-> [(Option Plot-Mark-Style) Font Stroke (-> FlRGBA FlRGBA) -> (Values Stroke Font FlRGBA Plot-Mark-Auto-Anchor)]
                                          [(Option Plot-Mark-Style) Font Stroke -> (Values Stroke Font Color Plot-Mark-Auto-Anchor)])
  (case-lambda
    [(self fallback-font fallback-pen)
     (if (or self)
         (values (or (plot-mark-style-pin-stroke self) fallback-pen)
                 (or (plot-mark-style-font self) fallback-font)
                 (or (plot-mark-style-color self) (stroke-color fallback-pen))
                 (plot-mark-style-anchor self))
         (plot-mark-visual-values (default-plot-mark-style) fallback-font fallback-pen))]
    [(self fallback-font fallback-pen adjust)
     (if (or self)
         (values (let ([pen (plot-mark-style-pin-stroke self)])
                   (if (and pen)
                       (stroke-adjust-color pen adjust)
                       fallback-pen))
                 (or (plot-mark-style-font self) fallback-font)
                 (let ([c (plot-mark-style-color self)])
                   (cond [(and c) (adjust (rgb* c))]
                         [else (stroke-color fallback-pen)]))
                 (plot-mark-style-anchor self))
         (plot-mark-visual-values (default-plot-mark-style) fallback-font fallback-pen adjust))]))

(define plot-mark-vector-values : (-> (Option Plot-Mark-Style) Plot-Mark-Fallback-Angle Plot-Mark-Fallback-Angle
                                      (Values (Option Plot-Mark-Fallback-Vector) (Option Plot-Mark-Fallback-Vector)))
  (lambda [self pin-angle gap-angle]
    (if (or self)
        (values (plot-mark-fallback-vector-guard (plot-mark-style-pin-length self)
                                                 (plot-mark-style-pin-angle self)
                                                 pin-angle)
                (plot-mark-fallback-vector-guard (plot-mark-style-gap-length self)
                                                 (plot-mark-style-gap-angle self)
                                                 gap-angle))
        (plot-mark-vector-values (default-plot-mark-style) pin-angle gap-angle))))

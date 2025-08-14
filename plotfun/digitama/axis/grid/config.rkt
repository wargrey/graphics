#lang typed/racket/base

(provide (all-defined-out))

(require geofun/color)
(require geofun/digitama/paint/self)

(require "self.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-grid-visual-values : (case-> [(Option Plot-Grid-Style) (-> FlRGBA FlRGBA) -> (Values (Option Stroke) (Option Stroke))]
                                          [(Option Plot-Grid-Style) -> (Values (Option Stroke) (Option Stroke))])
  (case-lambda
    [(self)
     (if (or self)
         (values (plot-grid-style-major-stroke self)
                 (plot-grid-style-minor-stroke self))
         (values #false #false))]
    [(self adjust)
     (if (or self)
         (let ([major-pen (plot-grid-style-major-stroke self)]
               [minor-pen (plot-grid-style-minor-stroke self)])
           (values (and major-pen (stroke-adjust-color major-pen adjust))
                   (and minor-pen (stroke-adjust-color minor-pen adjust))))
         (values #false #false))]))

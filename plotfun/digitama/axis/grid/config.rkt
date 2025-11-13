#lang typed/racket/base

(provide (all-defined-out))

(require geofun/color)
(require geofun/digitama/paint/self)

(require "self.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-grid-visual-values : (case-> [(Option Plot-Grid-Style) (U Index (-> Real Real Index)) (-> FlRGBA FlRGBA)
                                                                    -> (Values (Option Pen) (Option Pen)
                                                                               (U Index (-> Real Real Index)))]
                                          [(Option Plot-Grid-Style) (U Index (-> Real Real Index))
                                                                    -> (Values (Option Pen) (Option Pen)
                                                                               (U Index (-> Real Real Index)))])
  (case-lambda
    [(self minor-count)
     (if (or self)
         (values (plot-grid-style-major-stroke self)
                 (plot-grid-style-minor-stroke self)
                 (or (plot-grid-style-minor-count self)
                     minor-count))
         (values #false #false minor-count))]
    [(self minor-count adjust)
     (if (or self)
         (let ([major-pen (plot-grid-style-major-stroke self)]
               [minor-pen (plot-grid-style-minor-stroke self)])
           (values (and major-pen (pen-adjust-color major-pen adjust))
                   (and minor-pen (pen-adjust-color minor-pen adjust))
                   (or (plot-grid-style-minor-count self)
                       minor-count)))
         (values #false #false minor-count))]))

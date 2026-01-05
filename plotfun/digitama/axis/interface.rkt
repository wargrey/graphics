#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require geofun/font)
(require geofun/digitama/base)
(require geofun/digitama/self)
(require geofun/digitama/richtext/self)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Plot-Axis-Tick->Sticker (-> (Option Symbol) Geo-Rich-Text Font Color (U Geo Void False)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-plot-desired-ticks : (Parameterof Positive-Index) (make-parameter 7))
(define default-plot-real-tick-steps : (Parameterof (Listof Positive-Index)) (make-parameter (list 1 2 4 5)))
(define default-plot-axis-length : (Parameterof Real) (make-parameter 400.0))
(define default-plot-axis-unit-length : (Parameterof (Option Real+%)) (make-parameter #false))

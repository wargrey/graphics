#lang typed/racket/base

(require geofun/flowchart)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-culumon! fcht [64 64] #:-
  (d-> 1 'cin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (require geofun/vector)
  (require geofun/diagram)
  (require geofun/digitama/diagram/flowchart)

  (default-stroke (desc-stroke #:color 'crimson #:width 2.0))
  
  (reverse (flow-chart-edges fcht))
  fcht)

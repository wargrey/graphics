#lang typed/racket/base

(require plotfun/cartesian)
(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (geo-hb-append #:gapsize 32.0
                 (plot-cartesian #:x-range (cons -5 4) #:y-range (cons -5 4) #:style (make-plot-axis-style #:digit-position +1.2))
                 (plot-cartesian #:x-range (cons -5 4) #:y-range (cons -5 4) #:style (make-plot-axis-style #:digit-position -1.2 #:tick-placement 'negative))
                 (plot-cartesian #:x-range (cons -5 4) #:y-range (cons -5 4) #:style (make-plot-axis-style #:digit-position -1.2 #:tick-placement 'center)))

  (plot-cartesian #:x-ticks (plot-fixed-ticks (list 0 1 3 5 7 9))
                  #:y-ticks (plot-fixed-ticks (list 0 2 4 6 8 9))
                  #:style (make-plot-axis-style #:digit-position -1.2))

  (plot-cartesian #:x-range (cons -5 +5) #:y-range (cons -1 16) #:unit-length '(10 %)))

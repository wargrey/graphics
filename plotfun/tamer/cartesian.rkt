#lang typed/racket/base

(require plotfun/cartesian)
(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (geo-hb-append #:gapsize 32.0
                 (plot-cartesian #:x-range (cons -5 4) #:y-range (cons -5 4) #:style (make-plot-axis-style #:digit-position +1.2))
                 (plot-cartesian #:x-range (cons -5 4) #:y-range (cons -5 4) #:style (make-plot-axis-style #:digit-position -1.2 #:tick-placement 'negative))
                 (plot-cartesian #:x-range (cons -5 4) #:y-range (cons -5 4) #:style (make-plot-axis-style #:digit-position -1.2 #:tick-placement 'cross)))

  (geo-hb-append #:gapsize 32.0
                 (plot-cartesian #:screen? #f
                                 #:style (make-plot-axis-style #:digit-position -1.2 #:label-placement 'digit-mirror)
                                 #:x-ticks (plot-fixed-ticks (list 0 1 3 5 7 9))
                                 #:y-ticks (plot-fixed-ticks (list 0 2 4 6 8 9)))
                 (plot-cartesian #:screen? #t
                                 #:style (make-plot-axis-style #:digit-position -1.2 #:label-placement 'digit-mirror)
                                 #:x-ticks (plot-fixed-ticks (list 0 1 3 5 7 9))
                                 #:y-ticks (plot-fixed-ticks (list 0 2 4 6 8 9))))

  (geo-hb-append #:gapsize 32.0
                 (plot-cartesian #:screen? #f #:x-range (cons -5 +5) #:y-range (cons -1 16) #:unit-length '(10 %))
                 (plot-cartesian #:screen? #t #:x-range (cons -5 +5) #:y-range (cons -1 16) #:unit-length '(10 %))))

#lang typed/racket/base

(require plotfun/cartesian)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (plot-cartesian #:x-range (cons -5 4) #:y-range (cons -5 4) #:style (make-plot-axis-style #:digit-position +0.618))
  
  (plot-cartesian #:x-range (cons -5 4) #:y-range (cons -5 4) #:style (make-plot-axis-style #:digit-position -0.618))

  (plot-cartesian #:x-range (list 0 1 3 5 7 9) #:y-range (list 0 2 4 6 8 9) #:style (make-plot-axis-style #:digit-position -0.618)))

#lang typed/racket/base

(require plotfun/cartesian)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (plot-cartesian #:x-range (cons 0 9) #:y-range (cons 0 9) #:style (make-plot-axis-style #:digit-position -0.618))
  
  (plot-cartesian #:x-range (cons -5 4) #:y-range (cons -5 4) #:style (make-plot-axis-style #:digit-position +0.618))
  
  (plot-cartesian #:x-range (cons -5 4) #:y-range (cons -5 4) #:style (make-plot-axis-style #:digit-position -0.618)))

#lang typed/racket/base

(require plotfun/axis)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (plot-cartesian #:xtick-range (cons 0 9) #:ytick-range (cons 0 9) #:tick-digit-position -0.618
                  360 0.0)
  
  (plot-cartesian #:xtick-range (cons -5 4) #:ytick-range (cons -5 4) #:tick-digit-position +0.618
                  360 0.5+0.5i)
  
  (plot-cartesian #:xtick-range (cons -5 4) #:ytick-range (cons -5 4) #:tick-digit-position -0.618
                  360 0.5+0.5i))

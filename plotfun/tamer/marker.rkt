#lang typed/racket/base

(require geofun/vector)

(require plotfun/axis)
(require plotfun/cartesian)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (plot-integer-axis #:range (cons -5 5)
                     #:tip (make-plot-axis-tip-style #:positive-shape #false #:negative-shape #false #:margin 0.0+0.0i))
  
  (plot-integer-axis #:range (cons -5 5) #:label "P"
                     #:tip (make-plot-axis-tip-style))
  
  (plot-integer-axis #:range (cons -5 5) #:label (cons "N" "P")
                     #:tip (make-plot-axis-tip-style #:negative-shape default-axis-arrow #:margin -0.1-0.1i))

  (plot-cartesian #:x-range (cons -5 5) #:y-range (cons -5 5)
                  #:x-label (cons "n" "x") #:y-label (cons "m" "y")
                  #:x-tip (make-plot-axis-tip-style #:negative-shape default-axis-arrow #:margin -0.1-0.1i)))

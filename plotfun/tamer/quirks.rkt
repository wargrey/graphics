#lang typed/racket/base

(require plotfun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (plot-integer-axis #:label "default")
  (plot-axis #:range (cons -inf.0 +inf.0) #:label "infinity")
  (plot-axis #:ticks (plot-fixed-ticks (list 3.14)) #:label "R")
  (plot-axis #:range (cons -5 5) #:ticks (plot-fixed-ticks (list 3.14)) #:label "R")

  (plot-cartesian #:x-range (cons 0 0) #:unit-length -0.1 #:x-label "Zero"
                  #:x-tip (make-plot-axis-tip-style #:margin -0.01-0.06i))
  
  (plot-cartesian #:x-range (cons 0 0) #:unit-length -0.1 #:x-label "x"
                  #:style (make-plot-axis-style #:label-position 'digit))
  
  (plot-cartesian #:width +inf.0 #:height 0))

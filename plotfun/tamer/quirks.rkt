#lang typed/racket/base

(require plotfun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (plot-integer-line #:label "default")
  (plot-real-line #:range (cons -inf.0 +inf.0) #:label "infinity")
  (plot-real-line #:ticks (plot-fixed-ticks (list 3.14)) #:label "R")
  (plot-real-line #:range (cons -5 5) #:ticks (plot-fixed-ticks (list 3.14)) #:label "R")

  (plot-cartesian #:x-range (cons 0 0) #:unit-length '(10 %) #:x-label "Zero"
                  #:style (make-plot-axis-style #:tip (make-plot-axis-tip-style #:positive-margin '(10 %) #:negative-margin '(6 %))))
  
  (plot-cartesian #:x-range (cons 0 0) #:unit-length '(10 %) #:x-label "x"
                  #:style (make-plot-axis-style #:label-placement 'digit))
  
  (plot-cartesian #:width +inf.0 #:height 0))

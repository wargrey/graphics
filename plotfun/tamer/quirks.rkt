#lang typed/racket/base

(require plotfun)
(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (plot-integer-line #:label "default")
  (plot-real-line #:range (cons -inf.0 +inf.0) #:label "infinity")
  (plot-real-line #:ticks (plot-fixed-ticks (list 3.14)) #:label "R")
  (plot-real-line #:range (cons -5 5) #:ticks (plot-fixed-ticks (list 3.14)) #:label "R")

  (plot-cartesian #:x-range (cons -5 5) #:unit-length (&% 120) #:x-label "Zero"
                  #:style (make-plot-axis-style #:tip (make-plot-axis-tip-style #:positive-margin (&% 10) #:negative-margin (&% 6))))
  
  (plot-cartesian #:x-range (cons 0 0) #:unit-length (&% 10) #:x-label "Zero"
                  #:style (make-plot-axis-style #:tip (make-plot-axis-tip-style #:positive-margin (&% 10) #:negative-margin (&% 6))))

  (plot-cartesian #:y-down? #t
                  #:x-range (cons 0 0) #:unit-length (&% 10) #:x-label "Zero"
                  #:style (make-plot-axis-style #:tip (make-plot-axis-tip-style #:positive-margin (&% 10) #:negative-margin (&% 6))))
  
  (plot-cartesian #:x-range (cons 0 0) #:unit-length (&% 10)
                  #:style (make-plot-axis-style #:label-placement 'digit))

  (plot-cartesian #:y-down? #t
                  #:x-range (cons 0 0) #:unit-length (&% 10)
                  #:style (make-plot-axis-style #:label-placement 'digit))
  
  (plot-cartesian #:x-range (cons 0 0) #:unit-length (&% 10) #:x-label "t" #:y-label "s"
                  #:style (make-plot-axis-style #:label-placement 'mirror))
  
  (plot-cartesian #:width +inf.0 #:height 0)

  (plot-cartesian
   #:x-ticks (plot-symbol-ticks* -3/2 3/2 1/2 1)
   #:y-ticks (plot-real-ticks*)
   #:y-range (cons -1 1)
   #:hide-y-axis? #true)
  
  (plot-cartesian
   #:x-ticks (plot-symbol-ticks* -3/2 3/2 1/2 1)
   #:y-ticks (plot-real-ticks*)
   #:y-range (cons -1 1)
   #:hide-x-axis? #true)
  
  (plot-cartesian
   #:x-ticks (plot-symbol-ticks* -3/2 3/2 1/2 1)
   #:y-ticks (plot-real-ticks*)
   #:y-range (cons -1 1)
   #:hide-x-axis? #true
   #:hide-y-axis? #true))

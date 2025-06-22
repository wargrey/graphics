#lang typed/racket/base

(require geofun/vector)

(require plotfun/axis)
(require plotfun/cartesian)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (plot-integer-axis #:range (cons -5 5)
                     #:style (make-plot-axis-style #:tip plot-no-tip))
  
  (plot-integer-axis #:range (cons -5 5) #:label "P")
  
  (plot-integer-axis #:range (cons -5 5) #:label (cons "N" "P")
                     #:style (make-plot-axis-style #:tip plot-bi-tip))

  (plot-cartesian #:x-range (cons -5 5) #:y-range (cons -5 5)
                  #:x-label (cons "n" "x") #:y-label (cons "m" "y")
                  #:style (make-plot-axis-style #:tip plot-bi-tip)))

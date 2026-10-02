#lang typed/racket/base

(require plotfun)

(require "line.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define strict-style (make-plot-axis-style #:tip plot-strict-tip))
  (define flip-style (make-plot-axis-style #:label-placement 'digit))

  (plot-integer-line #:mark-template (plot-template 0.0 #:shape #false #:desc succ-desc)
                     #:unit-length (&% 10)
                     #:exclude-zero? #false
                     #:hide-axis? #true
                     #:label "n"
                     (list 0 1 2 3 4 5 6
                           (plot-integer 7 #:gap-length 0.0 #:datum 'arrow)))

  (geo-hc-append #:gapsize 16.0
                 (geo-vc-append #:gapsize 8.0
                                (plot-cartesian #:y-down? #f #:x-range (cons 0 0) #:unit-length (&% 10) #:x-label "Zero" #:style strict-style)
                                (plot-cartesian #:y-down? #t #:x-range (cons 0 0) #:unit-length (&% 10) #:x-label "Zero" #:style strict-style))

                 (geo-vc-append #:gapsize 8.0
                                (plot-cartesian #:y-down? #f #:x-range (cons 0 0) #:unit-length (&% 10) #:style flip-style)
                                (plot-cartesian #:y-down? #t #:x-range (cons 0 0) #:unit-length (&% 10) #:style flip-style)))

  (geo-hc-append #:gapsize 16.0
                 (plot-cartesian #:y-range (cons -1 1) #:x-range (cons 0 5)
                                 #:x-label "t" #:x-unit-desc "s"
                                 #:y-label "s" #:y-unit-desc "m")
                 (plot-cartesian #:y-range (cons -1 1) #:x-range (cons 0 5)
                                 #:style strict-style
                                 #:x-label "t" #:x-unit-desc "s"
                                 #:y-label "s" #:y-unit-desc "m"))

  (geo-hc-append #:gapsize 8.0
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
                  #:x-ticks (plot-symbol-ticks* 0 3/2 1/2 1)
                  #:y-ticks (plot-real-ticks*)
                  #:y-range (cons 0 1)
                  #:O #\0)))

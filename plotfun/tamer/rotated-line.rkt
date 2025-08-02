#lang typed/racket/base

(require plotfun)
(require geofun/vector)

(define reals : (Listof Real) (list -1/2 1/2))
(define range : (Pairof Real Real) (cons -2 8))
(define label : (Pairof String String) (cons "f = -20N" "F = 80N"))

(define label@digit
  (make-plot-axis-style #:label-placement 'digit
                        #:tip (make-plot-axis-tip-style #:positive-margin '(8 %) #:negative-margin '(8 %))))

(define label@mirro
  (make-plot-axis-style #:label-placement 'mirro #:tick-placement 'negative
                        #:tip (make-plot-axis-tip-style #:positive-margin 0 #:negative-margin 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (geo-vl-append #:gapsize 8.0
                 (geo-frame (plot-real-line reals #:range range #:label label #:rotate 0.0000 #:style label@digit))
                 (geo-frame (plot-real-line reals #:range range #:label label #:rotate 0.0000 #:style label@mirro))
                 (geo-frame (plot-real-line reals #:range range #:label label #:rotate 0.0000)))
  
  (geo-vl-append #:gapsize 8.0
                 (geo-frame (plot-real-line reals #:range range #:label label #:rotate (+ pi) #:style label@digit))
                 (geo-frame (plot-real-line reals #:range range #:label label #:rotate (+ pi) #:style label@mirro))
                 (geo-frame (plot-real-line reals #:range range #:label label #:rotate (+ pi))))
  
  (geo-hb-append #:gapsize 32.0
                 (geo-frame (plot-real-line reals #:range range #:label label #:rotate (- pi/2) #:style label@digit))
                 (geo-frame (plot-real-line reals #:range range #:label label #:rotate (+ pi/2) #:style label@digit))
                 (geo-frame (plot-real-line reals #:range range #:label label #:rotate (- pi/2) #:style label@mirro))
                 (geo-frame (plot-real-line reals #:range range #:label label #:rotate (+ pi/2) #:style label@mirro))
                 (geo-frame (plot-real-line reals #:range range #:label label #:rotate (- pi/2)))
                 (geo-frame (plot-real-line reals #:range range #:label label #:rotate (+ pi/2))))

  (geo-hb-append #:gapsize 32.0
                 (geo-frame (plot-real-line reals #:range range #:label label #:rotate (- pi/4) #:style label@digit))
                 (geo-frame (plot-real-line reals #:range range #:label label #:rotate (+ pi/4) #:style label@digit))
                 (geo-frame (plot-real-line reals #:range range #:label label #:rotate (- pi/4) #:style label@mirro))
                 (geo-frame (plot-real-line reals #:range range #:label label #:rotate (+ pi/4) #:style label@mirro))
                 (geo-frame (plot-real-line reals #:range range #:label label #:rotate (- pi/4)))
                 (geo-frame (plot-real-line reals #:range range #:label label #:rotate (+ pi/4))))
  
  (geo-hb-append #:gapsize 32.0
                 (geo-frame (plot-real-line reals #:range range #:label label #:rotate (- 3pi/4) #:style label@digit))
                 (geo-frame (plot-real-line reals #:range range #:label label #:rotate (+ 3pi/4) #:style label@digit))
                 (geo-frame (plot-real-line reals #:range range #:label label #:rotate (- 3pi/4) #:style label@mirro))
                 (geo-frame (plot-real-line reals #:range range #:label label #:rotate (+ 3pi/4) #:style label@mirro))
                 (geo-frame (plot-real-line reals #:range range #:label label #:rotate (- 3pi/4)))
                 (geo-frame (plot-real-line reals #:range range #:label label #:rotate (+ 3pi/4)))))

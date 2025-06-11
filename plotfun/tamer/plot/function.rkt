#lang typed/racket

(require geofun/vector)
(require plotfun/cartesian)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define discrete-floor : (-> Real (Option Real))
  (lambda [x]
    (define sx (* (- x (round x)) 100.0))

    (and (not (zero? (floor sx)))
         (floor x))))

(define discrete-ceiling : (-> Real (Option Real))
  (lambda [x]
    (define sx (* (- x (round x)) 100.0))

    (and (not (zero? (floor sx)))
         (ceiling x))))

(define normal-dist : (-> Real (Option Real))
  (lambda [x]
    (/ (exp (/ (sqr x) -2.0))
       (sqrt 2pi))))

(define 1/x : (-> Real (Option Real))
  (lambda [x]
    (/ 1 x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define vtree : (Listof Plot-Visualizer)
    (list (function #:color 'grey #:dash 'long-dash values)
          (function discrete-floor -3.0)
          (function discrete-ceiling)
          (function cos)
          (function exp  -3 +1.2)
          (function cos  -1 +1)
          (function sqr  -2 +2)
          (function sqrt +0 +4)
          (function log  +1 #f)
          (function 1/x  -3 +0 #:fast-range (λ [[xmin : Real] [xmax : Real]] (cons xmin xmax)))))

(define white-cart (plot-cartesian #:background white vtree))
(define black-cart (plot-cartesian #:background black #:style (make-plot-axis-style #:color 'lightgray) vtree))
(define  grey-cart (plot-cartesian #:background grey  vtree))

(define carts (geo-hb-append #:gapsize 32.0 white-cart black-cart grey-cart))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  carts
  (plot-cartesian
   (function sin  -4 +4   -1   +1)
   (function sin  -3 +3 -4/5 +4/5)
   (function normal-dist)))

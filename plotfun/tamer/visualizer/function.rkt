#lang typed/racket

(require geofun/vector)
(require geofun/markup)
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

(define -1/x : (-> Real (Option Real))
  (lambda [x]
    (/ -1 x)))

(define (sin+n [n : Integer]) : (-> Real (Option Real))
  (procedure-rename
   (λ [[x : Real]]
     (sin (+ x (* n 0.4))))
   (string->symbol (format "sin(x+~a)" (* n 4/10)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define vtree : (Listof Plot-Visualizer)
    (list (function #:color 'grey #:dash 'long-dash values #:label #false)
          (function -1/x +0   +4 #:fast-range (λ [[xmin : Real] [xmax : Real]] (cons xmin xmax)) #:label "1/x")
          (function discrete-floor -3.0 #:label "floor(x)")
          (function discrete-ceiling #:label "ceiling(x)")
          (function normal-dist #:label "N(x)")
          (function tan  -3   -1.8)
          (function exp  -3   +1.2 #:label (<span> null "e" (<sup> "x")))
          (function sqr  -2   +2 #:label (<span> null "x" (<sup> "2")))
          (function sqrt -3   +4)
          (function log  +1/4 #f)))

(define white-cart (plot-cartesian #:background white vtree))
(define  grey-cart (plot-cartesian #:background grey  vtree))
(define black-cart (plot-cartesian #:background black vtree))

(define carts (geo-hb-append #:gapsize 32.0 white-cart grey-cart black-cart))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  carts

  (plot-cartesian
   #:x-label "f"
   (for/list : (Listof Plot-Visualizer) ([i (in-range 10)])
     (function (sin+n i) -4 +4 -1 +1))))

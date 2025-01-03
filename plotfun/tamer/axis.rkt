#lang typed/racket/base

(require geofun)
(require plotfun/axis)

(require racket/math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define number-sticker : Plot-Axis-Real->Sticker
  (lambda [id r datum unit font axis-color]
    (define c (rgb* 'black (/ (+ r 1.0) 10.0)))
    (define g (geo-vc-append (geo-text "+1" font #:color c)
                             (geo-arc (* unit 0.5) pi 0.0 #:stroke c #:ratio 0.618)))
    
    (cons (if (eq? datum 'arrow)
              (geo-pin* 1.0 0.56 0.5 0.5 g (geo-dart (* unit 0.1) (* pi 0.5) #:fill c #:stroke #false))
              g)
          'lc)))

(define time-sticker : Plot-Axis-Real->Sticker
  (lambda [id r datum unit font axis-color]
    (define c (rgb* 'black (/ (+ r 1.0) 10.0)))
    (define arrow (geo-arrow 4.0 (* unit 0.5) (* pi 0.5) #:fill c #:stroke #false))
    (define label (geo-text datum font #:color c))

    (if (< r 2)
        (geo-vc-append #:gapsize 4.0
                       arrow label)
        (geo-vc-append #:gapsize 4.0
                       arrow
                       (geo-pin* 0.5 0.25 0.5 0.5
                                 (geo-sandglass (* unit 0.618) #:fill c #:stroke #false)
                                 (geo-text (object-name fib) font #:color 'GhostWhite))
                       arrow
                       label))))

(define fib : (-> Integer Integer)
  (lambda [n]
    (if (< n 2) n
        (+ (fib (- n 1))
           (fib (- n 2))))))

(define number-line
  (plot-axis #:tick-range (cons 0 8) #:reals '(0 1 2 3 4 5 6 7 (8 arrow))
             #:real->sticker number-sticker
             #:axis-label "i"
             360 0.0))

(define time-line
  (plot-axis #:tick-range (cons 0 9) #:reals fib
             #:real-position -2.5 #:real-anchor 'ct
             #:real->sticker time-sticker
             #:axis-label "n"
             360 0.0))


(module+ main
  number-line
  time-line)

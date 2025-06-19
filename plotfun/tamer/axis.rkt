#lang typed/racket/base

(require geofun)
(require plotfun/axis)

(require racket/math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define number-sticker : Plot-Axis-Real->Sticker
  (lambda [id r datum unit font color]
    (define c (rgb* color (/ (+ r 1.0) 10.0)))
    (define g (geo-vc-append (geo-text "+1" font #:color c)
                             (geo-arc (* unit 0.5) pi 0.0 #:stroke c #:ratio 0.618)))

    (cons (if (equal? '(arrow) datum)
              (geo-pin* 1.0 0.56 0.5 0.5 g (geo-dart (* unit 0.1) (* pi 0.5) #:fill c #:stroke #false))
              g)
          'lc)))

(define time-sticker : Plot-Axis-Real->Sticker
  (lambda [id r datum unit font color]
    (define c (rgb* color (/ (+ r 1.0) 10.0)))
    (define arrow (geo-arrow 4.0 (* unit 0.5) (* pi 0.5) #:fill c #:stroke #false))
    (define label (geo-text datum font #:color c))

    (if (< r 2)
        (geo-vc-append #:gapsize 4.0 arrow label)
        (geo-vc-append #:gapsize 4.0
                       arrow
                       (geo-pin* 0.5 0.25 0.5 0.5
                                 (geo-sandglass (* unit 0.618) #:fill c #:stroke #false)
                                 (geo-scale (geo-text (object-name fib) font #:color 'GhostWhite) 0.618))
                       arrow
                       label))))

(define fib : (-> Integer Integer)
  (lambda [n]
    (if (< n 2) n
        (+ (fib (- n 1))
           (fib (- n 2))))))

(define number-line
  (plot-integer-axis #:integer->sticker number-sticker
                     #:style (make-plot-axis-style #:label-position 'axis)
                     #:unit-length -0.1
                     #:label "x"
                     '(-1 0 1 2 3 4 5 6 (7 arrow))))

(define time-line
  (plot-integer-axis #:range (cons -1 9)
                     #:unit-length -0.1
                     #:style (make-plot-axis-style #:label-position 'digit)
                     #:integer-style (make-plot-marker-style #:position -2.5 #:anchor 'ct)
                     #:integer->sticker time-sticker
                     #:label "n"
                     fib))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  number-line
  time-line

  (plot-integer-axis #:range (cons -5 5) #:label "N")
  (plot-integer-axis #:range (cons 0 10) #:label "N")
  (plot-axis #:range (cons 0 10) #:label "R")
  (plot-axis #:range (cons +1 +7/2) #:label "R")
  (plot-axis #:range (cons -7 -5/2) #:label "R")
  (plot-axis #:range (cons -70 30) #:label "R")
  (plot-axis #:range (cons -0.001 +0.007) #:label "R")
  (plot-axis #:range (cons -0.618 +0.618) #:label "R")
  (plot-axis #:ticks (plot-fixed-ticks -6.28 +6.29 3.14) #:label "R"))

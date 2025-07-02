#lang typed/racket/base

(require geofun/vector)
(require plotfun/line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define succ-desc : Plot-Mark->Description
  (lambda [pt datum font color transform]
    (define n (real->double-flonum (real-part pt)))

    (when (>= n 0.0)
      (define unit (real-part (- (transform (+ n 1.0) 0.0) (transform n 0.0))))
      (define c (rgb* color (/ (+ n 1.0) 8.0)))
      (define g (geo-vc-append (geo-text "+1" font #:color c)
                               (geo-arc (* unit 0.5) 3.4 6.0 #:stroke c #:ratio 1.618)))
      
      (make-geo-sticker (if (eq? 'arrow datum)
                            (geo-pin* 0.975 0.60 0.5 0.5 g (geo-dart (* unit 0.1) (* pi 0.375) #:fill c #:stroke #false))
                            g)
                        'lb
                        (make-rectangular 0.0 (* unit 0.25))))))

(define fib-desc : Plot-Mark->Description
  (lambda [pt datum font color transform]
    (define n (real->double-flonum (real-part pt)))
    (define unit (max (real-part (- (transform (+ n 1.0) 0.0) (transform n 0.0))) 0.0))
    (define c (rgb* color (/ (+ n 1.0) 10.0)))
    (define arrow (geo-arrow 4.0 (* unit 0.5) (* pi 0.5) #:fill c #:stroke #false))
    (define label (geo-text datum font #:color c))

    (if (< n 1)
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

(define real-line
  (plot-real-line #:style (make-plot-axis-style #:label-placement 'axis)
                  #:range (cons 0 6)
                  #:unit-length '(24 %)
                  #:label "R"
                  '(0 55/89 89/55 19/7 22/7)))

(define seq-line
  (plot-integer-line #:mark-template succ-desc
                     #:mark-style (make-plot-mark-style #:pin-length 0.0)
                     #:unit-length '(10 %)
                     #:exclude-zero? #false
                     #:label "n"
                     (list 0 1 2 3 4 5 6
                           (plot-integer 7 #:gap-length 0.0 #:datum 'arrow))))

(define time-line
  (plot-integer-line #:range (cons 0 9)
                     #:mark-template (plot-template '(220 %) pi/2 #:desc fib-desc #:pin? #false #:anchor 'ct)
                     #:unit-length '(10 %)
                     #:label "n"
                     fib))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  real-line
  seq-line
  time-line

  (plot-integer-line #:range (cons -5 5) #:label "N")
  (plot-integer-line #:range (cons 0 10) #:label "N")
  (plot-real-line #:range (cons 0 10) #:label "R")
  (plot-real-line #:range (cons +1 +7/2) #:label "R")
  (plot-real-line #:range (cons -7 -5/2) #:label "R")
  (plot-real-line #:range (cons -70 30) #:label "R")
  (plot-real-line #:range (cons -0.001 +0.007) #:label "R")
  (plot-real-line #:range (cons -0.618 +0.618) #:label "R")
  (plot-real-line #:ticks (plot-fixed-ticks -6.28 +6.29 3.14) #:label "R"))

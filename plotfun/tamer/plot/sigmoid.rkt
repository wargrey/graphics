#lang typed/racket

(require geofun/vector)
(require plotfun/cartesian)

(require colorspace/palette)
(require colorspace/misc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sigmoid #:k [k : Nonnegative-Flonum] [sig : (-> Flonum Flonum)]) : (-> Real Real)
  (lambda [x]
    (sig (real->double-flonum(* k x)))))

(define ΔL/log1 : (-> Real Real)
  (lambda [bgL]
    (* 0.25 (/ (log (max 0.0 (+ 1.0 (* 10.0 (- 1.0 bgL)))))
               (log 10.0)))))

(define ΔL/log2 : (-> Real Real)
  (lambda [bgL]
    (* 0.30 (log (max 0.0 (- 2.0 bgL))))))

(define chroma-L : (-> Real Real)
  (lambda [L]
    (cond [(< L 0.35) (+ 0.8 (* 0.4 L))]
          [(> L 0.65) (- 1.5 (* 0.5 L))]
          [else 1.2])))

(define chroma-C : (-> Real Real)
  (lambda [C]
    (/ 1.0 (+ 1.0 (* 2.0 C)))))

(define chroma-H : (-> Real Real)
  (lambda [H]
    (+ (* 0.25 (cos (degrees->radians (- H 240.0))))
       1.0)))

(define (chroma [C0 : Real] [bgL : Flonum] [bgC : Flonum]) : (-> Real Real)
  (lambda [x]
    (* (real->ok-chroma C0)
       (chroma-L bgL)
       (chroma-C bgC)
       (chroma-H x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define sigmoid.plot
  (geo-hb-append #:gapsize 16.0
                 (plot-cartesian
                  #:x-range (cons -1.0 1.0) #:y-range (cons 0.0 1.0)
                  #:x-label "L" #:y-label "logistic"
                  (function (sigmoid palette-sigmoid/logistic #:k 10.0))
                  (function (sigmoid palette-sigmoid/logistic #:k 20.0))
                  (function (sigmoid palette-sigmoid/logistic #:k 30.0))
                  (function (sigmoid palette-sigmoid/logistic #:k 40.0))
                  (function (sigmoid palette-sigmoid/logistic #:k 50.0)))
                 
                 (plot-cartesian
                  #:x-range (cons -1.0 1.0) #:y-range (cons 0.0 1.0)
                  #:x-label "L" #:y-label "tanh"
                  (function (sigmoid palette-sigmoid/tanh #:k 10.0))
                  (function (sigmoid palette-sigmoid/tanh #:k 20.0))
                  (function (sigmoid palette-sigmoid/tanh #:k 30.0))
                  (function (sigmoid palette-sigmoid/tanh #:k 40.0))
                  (function (sigmoid palette-sigmoid/tanh #:k 50.0)))
                 
                 (plot-cartesian
                  #:x-range (cons -1.0 1.0) #:y-range (cons 0.0 1.0)
                  #:x-label "L" #:y-label "algebraic"
                  (function (sigmoid palette-sigmoid/algebraic #:k 10.0))
                  (function (sigmoid palette-sigmoid/algebraic #:k 20.0))
                  (function (sigmoid palette-sigmoid/algebraic #:k 30.0))
                  (function (sigmoid palette-sigmoid/algebraic #:k 40.0))
                  (function (sigmoid palette-sigmoid/algebraic #:k 50.0)))))

(define brightness.plot
  (geo-hb-append #:gapsize 16.0
                 (plot-cartesian
                  #:x-range (cons 0.0 1.0) #:y-range (cons 0.0 1.0)
                  #:x-label "L" #:y-label "logistic"
                  (function (oklch-palette-sigmoid-interpolator palette-sigmoid/logistic #:k 10.0))
                  (function (oklch-palette-sigmoid-interpolator palette-sigmoid/logistic #:k 20.0))
                  (function (oklch-palette-sigmoid-interpolator palette-sigmoid/logistic #:k 30.0))
                  (function (oklch-palette-sigmoid-interpolator palette-sigmoid/logistic #:k 40.0))
                  (function (oklch-palette-sigmoid-interpolator palette-sigmoid/logistic #:k 50.0)))
                 
                 (plot-cartesian
                  #:x-range (cons 0.0 1.0) #:y-range (cons 0.0 1.0)
                  #:x-label "L" #:y-label "tanh"
                  (function (oklch-palette-sigmoid-interpolator palette-sigmoid/tanh #:k 10.0))
                  (function (oklch-palette-sigmoid-interpolator palette-sigmoid/tanh #:k 20.0))
                  (function (oklch-palette-sigmoid-interpolator palette-sigmoid/tanh #:k 30.0))
                  (function (oklch-palette-sigmoid-interpolator palette-sigmoid/tanh #:k 40.0))
                  (function (oklch-palette-sigmoid-interpolator palette-sigmoid/tanh #:k 50.0)))
                 
                 (plot-cartesian
                  #:x-range (cons 0.0 1.0) #:y-range (cons 0.0 1.0)
                  #:x-label "L" #:y-label "algebraic"
                  (function (oklch-palette-sigmoid-interpolator palette-sigmoid/algebraic #:k 10.0))
                  (function (oklch-palette-sigmoid-interpolator palette-sigmoid/algebraic #:k 20.0))
                  (function (oklch-palette-sigmoid-interpolator palette-sigmoid/algebraic #:k 30.0))
                  (function (oklch-palette-sigmoid-interpolator palette-sigmoid/algebraic #:k 40.0))
                  (function (oklch-palette-sigmoid-interpolator palette-sigmoid/algebraic #:k 50.0)))))

(define chroma.plot
  (plot-cartesian
   #:x-range (cons 0.0 360) #:y-range (cons 0.0 0.4) #:height 200.0
   #:x-label "H" #:y-label "C"
   (function (chroma 1/4 0.0 0.0))
   (function (chroma 1/4 0.5 0.0))
   (function (chroma 1/4 1.0 0.0))
   (function (chroma 2/5 0.0 0.0))
   (function (chroma 2/5 0.5 0.0))
   (function (chroma 2/5 1.0 0.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  sigmoid.plot
  brightness.plot
  chroma.plot)

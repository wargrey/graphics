#lang typed/racket

(require geofun/vector)
(require plotfun/cartesian)

(require colorspace/palette)
(require colorspace/misc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sigmoid #:k [k : Nonnegative-Flonum] [sig : (-> Flonum Flonum)]) : (-> Real Real)
  (procedure-rename
   (λ [[x : Real]] (sig (real->double-flonum(* k x))))
   (string->symbol (format "k = ~a" k))))

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
    (+ (* 0.3 (cos (degrees->radians (- H 240.0))))
       1.0)))

(define (chroma [C0 : Real] [bgL : Flonum] [bgC : Flonum]) : (-> Real Real)
  (procedure-rename
   (λ [[x : Real]] (* (real->ok-chroma C0)
                      (chroma-L bgL)
                      (chroma-C bgC)
                      (chroma-H x)))
   (string->symbol (format "C = ~a on ~a" C0 bgL))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define sigmoid.plot
  (parameterize ([default-plot-visualizer-label-position-range (cons 0.4 0.6)])
    (geo-hb-append #:gapsize 16.0
                   (plot-cartesian
                    #:x-range (cons -1.0 1.0) #:y-range (cons 0.0 1.0)
                    #:x-label "L" #:y-label "L" #:y-desc "logistic"
                    (function (sigmoid palette-sigmoid/logistic #:k 10.0))
                    (function (sigmoid palette-sigmoid/logistic #:k 20.0))
                    (function (sigmoid palette-sigmoid/logistic #:k 30.0))
                    (function (sigmoid palette-sigmoid/logistic #:k 40.0))
                    (function (sigmoid palette-sigmoid/logistic #:k 50.0)))
                   
                   (plot-cartesian
                    #:x-range (cons -1.0 1.0) #:y-range (cons 0.0 1.0)
                    #:x-label "L" #:y-label "L" #:y-desc "tanh"
                    (function (sigmoid palette-sigmoid/tanh #:k 10.0))
                    (function (sigmoid palette-sigmoid/tanh #:k 20.0))
                    (function (sigmoid palette-sigmoid/tanh #:k 30.0))
                    (function (sigmoid palette-sigmoid/tanh #:k 40.0))
                    (function (sigmoid palette-sigmoid/tanh #:k 50.0)))
                   
                   (plot-cartesian
                    #:x-range (cons -1.0 1.0) #:y-range (cons 0.0 1.0)
                    #:x-label "L" #:y-label "L" #:y-desc "algebraic"
                    (function (sigmoid palette-sigmoid/algebraic #:k 10.0))
                    (function (sigmoid palette-sigmoid/algebraic #:k 20.0))
                    (function (sigmoid palette-sigmoid/algebraic #:k 30.0))
                    (function (sigmoid palette-sigmoid/algebraic #:k 40.0))
                    (function (sigmoid palette-sigmoid/algebraic #:k 50.0))))))

(define brightness.plot
  (geo-hb-append #:gapsize 16.0
                 (plot-cartesian
                  #:x-range (cons 0.0 1.0) #:y-range (cons 0.0 1.0)
                  #:x-label "L" #:y-label "L" #:y-desc "logistic"
                  (function (oklch-palette-sigmoid-interpolator palette-sigmoid/logistic #:k 10.0))
                  (function (oklch-palette-sigmoid-interpolator palette-sigmoid/logistic #:k 20.0))
                  (function (oklch-palette-sigmoid-interpolator palette-sigmoid/logistic #:k 30.0))
                  (function (oklch-palette-sigmoid-interpolator palette-sigmoid/logistic #:k 40.0))
                  (function (oklch-palette-sigmoid-interpolator palette-sigmoid/logistic #:k 50.0)))
                 
                 (plot-cartesian
                  #:x-range (cons 0.0 1.0) #:y-range (cons 0.0 1.0)
                  #:x-label "L" #:y-label "L" #:y-desc "tanh"
                  (function (oklch-palette-sigmoid-interpolator palette-sigmoid/tanh #:k 10.0))
                  (function (oklch-palette-sigmoid-interpolator palette-sigmoid/tanh #:k 20.0))
                  (function (oklch-palette-sigmoid-interpolator palette-sigmoid/tanh #:k 30.0))
                  (function (oklch-palette-sigmoid-interpolator palette-sigmoid/tanh #:k 40.0))
                  (function (oklch-palette-sigmoid-interpolator palette-sigmoid/tanh #:k 50.0)))
                 
                 (plot-cartesian
                  #:x-range (cons 0.0 1.0) #:y-range (cons 0.0 1.0)
                  #:x-label "L" #:y-label "L" #:y-desc "algebraic"
                  (function (oklch-palette-sigmoid-interpolator palette-sigmoid/algebraic #:k 10.0))
                  (function (oklch-palette-sigmoid-interpolator palette-sigmoid/algebraic #:k 20.0))
                  (function (oklch-palette-sigmoid-interpolator palette-sigmoid/algebraic #:k 30.0))
                  (function (oklch-palette-sigmoid-interpolator palette-sigmoid/algebraic #:k 40.0))
                  (function (oklch-palette-sigmoid-interpolator palette-sigmoid/algebraic #:k 50.0)))))

(define brightness/vector.plot
  (parameterize ([default-plot-visualizer-label-position-range (cons 0.1 0.3)]
                 [default-plot-visualizer-label-placement 'flip])
    (plot-cartesian
     #:x-range (cons 0.0 1.0) #:y-range (cons 0.0 1.0)
     #:x-label "L" #:y-label "L" #:y-desc "logistic"
     (function (oklch-palette-sigmoid-interpolator
                #:smooth-vector #(0.4 0.3 0.5) #:name (format "~a" #(0.4 0.3 0.5))
                palette-sigmoid/logistic))
     (function (oklch-palette-sigmoid-interpolator
                #:smooth-vector #(0.5 0.4 0.6) #:name (format "~a" #(0.5 0.4 0.6))
                palette-sigmoid/logistic))
     (function (oklch-palette-sigmoid-interpolator
                #:smooth-vector #(0.6 0.5 0.7) #:name (format "~a" #(0.6 0.5 0.7))
                palette-sigmoid/logistic))
     (function (oklch-palette-sigmoid-interpolator
                #:smooth-vector #(0.7 0.6 0.8) #:name (format "~a" #(0.7 0.6 0.8))
                palette-sigmoid/logistic)))))

(define chroma.plot
  (parameterize ([default-plot-visualizer-label-position 1.0])
    (plot-cartesian
     #:x-range (cons 0.0 360) #:y-range (cons 0.0 0.4) #:height 200.0
     #:x-label "Hue" #:y-label "Chroma"
     #:mark-style (make-plot-mark-style #:pin-angle 0.0 #:gap-angle 0.0)
     #:y-grid-style (make-plot-grid-style #:minor-count 0)
     (function (chroma 1/4 0.0 0.0))
     (function (chroma 1/4 0.5 0.0))
     (function (chroma 1/4 1.0 0.0))
     (function (chroma 2/5 0.0 0.0))
     (function (chroma 2/5 0.5 0.0))
     (function (chroma 2/5 1.0 0.0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  sigmoid.plot
  brightness/vector.plot
  chroma.plot)

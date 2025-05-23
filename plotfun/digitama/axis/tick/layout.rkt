#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

(require "shared.rkt")
(require "../../arithmetics.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-real-tick-layout : (-> Real Real Positive-Byte Positive-Index (Listof Positive-Index) (Values (Listof Real) (Option Real)))
  (lambda [tmin tmax base desired-ticks divisors]
    (define R : Real (- tmax tmin))
    (define order : Positive-Exact-Rational (magnitude-order R base))
    (define epsilon : Positive-Exact-Rational (expt base (- (tick-precision tmin tmax))))
    
    (define-values (step0 num-diff)
      (for*/fold ([step : (U Real #f) #f]
                  [num-diff : Real +inf.0])
                 ([s (in-range (floor-log desired-ticks base) -2 -1)]
                  [d (in-list divisors)])
        (define scaled-step : Positive-Exact-Rational (/ order d (expt base s)))
        (define-values (refined-min refined-max refined-num) (plot-range-refine tmin tmax scaled-step))
        
        ;; don't count on endpoints for contours, but also good for other renderers
        (define final-num : Integer
          (- refined-num
             (if (< (abs (- refined-min tmin)) epsilon) 1 0)
             (if (< (abs (- refined-max tmax)) epsilon) 1 0)))
        
        (define refined-diff (abs (- final-num desired-ticks)))
        
        (if (<= refined-diff num-diff)
            (values scaled-step refined-diff)
            (values step num-diff))))

    (let ([step (or step0 (/ R desired-ticks))])
      (values (range tmin (+ tmax (* step 0.5)) step) step))))

(define plot-integer-tick-layout : (-> Integer Integer Positive-Byte Positive-Index (Values (Listof Integer) (Option Natural)))
  (lambda [tmin tmax base desired-ticks]
    (define R : Integer (- tmax tmin))
    (define Δinitialized : Exact-Rational (/ R desired-ticks))
    (define order : Positive-Exact-Rational (magnitude-order Δinitialized base))
    
    (define step : Natural
      (if (exact-integer? order)
          (let ([normalized (/ Δinitialized order)])
            (cond [(< normalized 3/2)  order]
                  [(< normalized 3) (* order 2)]
                  [(< normalized 7) (* order (if (= (remainder R 3) 0) 3 5))]
                  [else             (* order (if (= (remainder R 7) 0) 7 10))]))
          1))
    
    (values (range tmin (+ tmax 1) step) step)))

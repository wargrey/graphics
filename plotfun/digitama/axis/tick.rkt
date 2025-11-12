#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)
(require digimon/flonum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (R) plot-axis-metrics
  : (case-> [-> (U (∩ R Real) (Pairof (∩ R Real) (∩ R Real)) False) (Option (Pairof (∩ R Real) (∩ R Real)))
                (Option Real) Nonnegative-Flonum (Option Real+%)
                (Values (Option (Pairof (U R Zero) (U R Zero))) Flonum Nonnegative-Flonum)]
            [-> (Option (Pairof (∩ R Real) (∩ R Real))) (Option Real) Nonnegative-Flonum (Option Real+%)
                (Values (Option (Pairof (U R Zero) (U R Zero))) Flonum Nonnegative-Flonum)])
  (case-lambda
    [(tick-rng real-rng maybe-origin axis-length maybe-unit-length)
     (define maybe-ticks : (Option (Pairof (U (∩ R Real) Zero) (U (∩ R Real) Zero)))
       (cond [(or tick-rng) ((inst plot-tick-range R) tick-rng)]
             [(or real-rng) ((inst plot-tick-range* R) (car real-rng) (cdr real-rng))]
             [else #false]))

     (define unit-length : Nonnegative-Flonum
       (cond [(and maybe-unit-length) (~length maybe-unit-length axis-length)]
             [(pair? maybe-ticks) (plot-auto-unit-length axis-length maybe-ticks)]
             [else (* axis-length 0.1)]))

     (define origin : Flonum
       (cond [(rational? maybe-origin) (real->double-flonum maybe-origin)]
             [(pair? maybe-ticks) (plot-auto-origin maybe-ticks unit-length axis-length)]
             [else 0.5]))

     (values maybe-ticks origin unit-length)]
    [(real-rng maybe-origin axis-length maybe-unit-length)
     ((inst plot-axis-metrics R) #false real-rng maybe-origin axis-length maybe-unit-length)]))

(define #:forall (R C) plot-axis-metrics*
  : (case-> [-> (Option (Pairof (∩ R Real) (∩ R Real))) (Option Real) Nonnegative-Flonum (Option Real+%) (-> Nonnegative-Flonum C)
                (Values (Option (Pairof (U R Zero) (U R Zero))) Flonum Nonnegative-Flonum C)]
            [-> (U (∩ R Real) (Pairof (∩ R Real) (∩ R Real)) False) (Option (Pairof (∩ R Real) (∩ R Real)))
                (Option Real) Nonnegative-Flonum (Option Real+%) (-> Nonnegative-Flonum C)
                (Values (Option (Pairof (U R Zero) (U R Zero))) Flonum Nonnegative-Flonum C)])
  (case-lambda
    [(real-rng maybe-origin axis-length maybe-unit-length unit-transform)
     (define-values (ticks origin flunit) ((inst plot-axis-metrics R) #false real-rng maybe-origin axis-length maybe-unit-length))
     (values ticks origin flunit (unit-transform flunit))]
    [(tick-rng real-rng maybe-origin axis-length maybe-unit-length unit-transform)
     (define-values (ticks origin flunit) ((inst plot-axis-metrics R) tick-rng real-rng maybe-origin axis-length maybe-unit-length))
     (values ticks origin flunit (unit-transform flunit))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (R) plot-tick-range* : (-> (∩ R Real) (∩ R Real)
                                            (Option (Pairof (U (∩ R Real) Zero) (U (∩ R Real) Zero))))
  (lambda [lft rgt]
    (cond [(and (rational? lft) (rational? rgt))
           (cond [(< lft rgt) (cons lft rgt)]
                 [(> lft rgt) (cons rgt lft)]
                 [(negative? lft) (cons lft 0)]
                 [(positive? lft) (cons 0 lft)]
                 [else #false])]
          [(rational? lft) ((inst plot-tick-range* R) lft lft)]
          [(rational? rgt) ((inst plot-tick-range* R) rgt rgt)]
          [else #false])))
  
(define #:forall (R) plot-tick-range : (-> (U (∩ R Real) (Pairof (∩ R Real) (∩ R Real)))
                                           (Option (Pairof (U (∩ R Real) Zero) (U (∩ R Real) Zero))))
  (lambda [rng]
     (cond [(real? rng) ((inst plot-tick-range* R) rng rng)]
           [else ((inst plot-tick-range* R) (car rng) (cdr rng))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-auto-origin : (-> (Pairof Real Real) Nonnegative-Real Nonnegative-Flonum Flonum)
  (lambda [ticks unit-length fllength]
    (/ (real->double-flonum (* (- (car ticks))
                               unit-length))
       fllength)))

(define plot-auto-unit-length : (-> Nonnegative-Flonum (Pairof Real Real) Nonnegative-Flonum)
  (lambda [fllength ticks]
    (max (/ fllength
            (- (cdr ticks)
               (car ticks)))
         0.0)))

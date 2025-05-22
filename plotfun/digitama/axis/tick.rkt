#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)
(require racket/list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (R C) plot-axis-metrics
  : (case-> [-> (U (∩ R Real) (Pairof (∩ R Real) (∩ R Real)) (Listof (∩ R Real)) False) (Option (Pairof (∩ R Real) (∩ R Real)))
                (Option Real) Nonnegative-Flonum (Option Real)
                (Values (Option (Pairof (U R Zero) (U R Zero))) Real Nonnegative-Flonum)]
            [-> (U (∩ R Real) (Pairof (∩ R Real) (∩ R Real)) (Listof (∩ R Real)) False) (Option (Pairof (∩ R Real) (∩ R Real)))
                (Option Real) Nonnegative-Flonum (Option Real) (-> Nonnegative-Flonum C)
                (Values (Option (Pairof (U R Zero) (U R Zero))) Real C)])
  (case-lambda
    [(tick-rng real-rng maybe-origin axis-length maybe-unit-length)
     (define ticks : (Option (Pairof (U R Zero) (U R Zero)))
       (cond [(or tick-rng) ((inst plot-tick-range R) tick-rng)]
             [(or real-rng) ((inst plot-tick-range* R) (car real-rng) (cdr real-rng))]
             [else #false]))
     
     (cond [(and maybe-origin maybe-unit-length) (values ticks maybe-origin (~length maybe-unit-length axis-length))]
           [(and (pair? ticks) (real? (car ticks)) (real? (cdr ticks)))
            (let* ([unit-length (plot-auto-unit-length axis-length maybe-unit-length ticks)])
              (values ticks (or maybe-origin (plot-auto-origin ticks unit-length axis-length)) unit-length))]
           [else (values ticks (or maybe-origin 0.5) (min (~length (or maybe-unit-length -0.1) axis-length)))])]
    [(tick-rng real-rng maybe-origin axis-length maybe-unit-length unit-transform)
     (define-values (ticks origin flunit) ((inst plot-axis-metrics R) tick-rng real-rng maybe-origin axis-length maybe-unit-length))
     (values ticks origin (unit-transform flunit))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (R) plot-tick-range* : (-> (∩ R Real) (∩ R Real) (Option (Pairof (U R Zero) (U R Zero))))
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
  
(define #:forall (R) plot-tick-range : (-> (U (∩ R Real) (Listof (∩ R Real)) (Pairof (∩ R Real) (∩ R Real))) (Option (Pairof (U R Zero) (U R Zero))))
  (lambda [rng]
     (cond [(real? rng) ((inst plot-tick-range* R) rng rng)]
           [(null? rng) #false]
           [(null? (cdr rng)) ((inst plot-tick-range* R) (car rng) (car rng))]
           [(not (list? rng)) ((inst plot-tick-range* R) (car rng) (cdr rng))]
           [else (let ([sorted-rng (sort rng <)])
                   ((inst plot-tick-range* R) (car sorted-rng) (last sorted-rng)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-auto-origin : (-> (Pairof Real Real) Nonnegative-Real Nonnegative-Flonum Flonum)
  (lambda [ticks unit-length fllength]
    (/ (real->double-flonum (* (- (car ticks)) unit-length)) fllength)))

(define plot-auto-unit-length : (-> Nonnegative-Flonum (Option Real) (Pairof Real Real) Nonnegative-Flonum)
  (lambda [fllength maybe-unit-length ticks]
    (if (not maybe-unit-length)
        (max (/ fllength (- (cdr ticks) (car ticks))) 0.0)
        (~length maybe-unit-length fllength))))

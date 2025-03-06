#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)
(require racket/math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-axis-ticks* : (-> Real Real (Listof Integer)) 
  (lambda [left right]
    (cond [(< left right) (range (exact-ceiling left) (+ (exact-floor right) 1))]
          [(> left right) (range (exact-ceiling right) (+ (exact-floor left) 1))]
          [(< left 0) (range (exact-ceiling left) 1)]
          [(> left 0) (range 0 (+ (exact-floor left) 1))]
          [else null])))

(define plot-axis-ticks : (-> (U Real (Listof Real) (Pairof Real Real)) (Listof Integer))
  (lambda [rng]
     (cond [(real? rng) (plot-axis-ticks* rng rng)]
           [(null? rng) null]
           [(null? (cdr rng)) (plot-axis-ticks* (car rng) (car rng))]
           [(not (list? rng)) (plot-axis-ticks* (car rng) (cdr rng))]
           [else (let ([1st (car rng)]
                       [2nd (cadr rng)])
                   (let pseudo-sort ([left  (min 1st 2nd)]
                                     [right (max 1st 2nd)]
                                     [rest (cddr rng)])
                     (cond [(null? rest) (plot-axis-ticks* left right)]
                           [else (let-values ([(head tail) (values (car rest) (cdr rest))])
                                   (pseudo-sort (min left head)
                                                (max right head)
                                                tail))])))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (T) plot-cartesian-settings : (case-> [Complex -> (Values Flonum Flonum)]
                                                       [Complex (-> Real T) -> (Values T T)])
  (case-lambda
    [(config) (plot-cartesian-settings config real->double-flonum)]
    [(config real->datum)
     (define x (real->datum (real-part config)))
     (define y (if (real? config) x (real->datum (imag-part config))))
     
     (values x y)]))

(define #:forall (T) plot-cartesian-settings* : (case-> [Complex (case-> [Real -> T] [Real T -> T]) -> (Values T T)]
                                                        [Complex (-> Real T T) T -> (Values T T)])
  (case-lambda
    [(config real->datum)
     (define x (real->datum (real-part config)))
     (define y (if (real? config) x (real->datum (imag-part config) x)))
     
     (values x y)]
    [(config real->datum 100%)
     (define x (real->datum (real-part config) 100%))
     (define y (if (real? config) x (real->datum (imag-part config) 100%)))
     
     (values x y)]))

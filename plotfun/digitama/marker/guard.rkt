#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)
(require racket/math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Plot-Mark-Vector (Pairof (Option Length+%) Flonum))

(define-type Plot-Mark-Fallback-Angle (U Real (-> Real (Option Real))))
(define-type Plot-Mark-Fallback-Vector (Pairof Length+% Plot-Mark-Fallback-Angle))

(define plot-mark-null-vector : Plot-Mark-Vector (cons #false +nan.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-mark-vector : (-> Length+% Real Plot-Mark-Vector)
  (lambda [length angle]
    (cons (and (&rational? length) length)
          (if (rational? angle) (real->double-flonum angle) +nan.0))))

(define plot-mark-pin-vector : (-> Boolean (Option Length+%) (Option Real) (Option Plot-Mark-Vector))
  (lambda [pin? length angle]
    (and pin?
         (or length angle)
         (plot-mark-vector (or length +nan.0)
                           (or  angle +nan.0)))))

(define plot-mark-fallback-vector-guard : (-> Length+% Real Plot-Mark-Fallback-Angle (Option Plot-Mark-Fallback-Vector))
  (lambda [length maybe-angle dynamic-angle]
    (and (&rational? length)
         (cond [(&rational? maybe-angle) (cons length (real->double-flonum maybe-angle))]
               [(procedure? dynamic-angle) (cons length dynamic-angle)]
               [(rational? dynamic-angle) (cons length dynamic-angle)]
               [else #false]))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-mark-vector-guard : (-> (Option Plot-Mark-Vector) (Option Plot-Mark-Fallback-Vector) Nonnegative-Flonum Real Flonum
                                     (Values (Option Float-Complex) (Option Flonum)))
  (lambda [self fallback-vctr 100% x rotate]
    (define-values (length fb.rad)
      (cond [(list? fallback-vctr)
             (values (list (car fallback-vctr) (caddr fallback-vctr))
                     (cadr fallback-vctr))]
            [(pair? fallback-vctr)
             (values (car fallback-vctr) (cdr fallback-vctr))]
            [else (values +nan.0 +nan.0)]))

    (define (guard [guarded-len : Flonum] [rad : Flonum]) : (Values (Option Float-Complex) (Option Flonum))
      (define guarded-rad
        (cond [(rational? rad) (+ rad rotate)]
              [(procedure? fb.rad) (+ (real->double-flonum (or (fb.rad x) +nan.0)) rotate)]
              [else (+ (real->double-flonum fb.rad) rotate)]))

      (values (and (rational? guarded-len)
                   (> guarded-len 0.0)
                   (rational? guarded-rad)
                   (make-polar guarded-len guarded-rad))
              (and (rational? guarded-rad)
                   (angle (make-polar 1.0 guarded-rad)))))
    
    (if (pair? self)
        (guard (~dimension (or (car self) length) 100%)
               (cdr self))
        (values #false #false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-mark-point-guard : (case-> [-> Complex (-> Real Real) Positive-Fixnum Positive-Index
                                            Real Real Real Real Real (Pairof Real Real)
                                            (Option Complex)]
                                        [-> Complex (-> Real Real) Positive-Fixnum Positive-Index
                                            Real Real Real Real Real Real Real (Pairof Real Real)
                                            (Option Complex)])
  (case-lambda
    [(pt fx idx total xmin xmax ymin ymax frac frac-rng)
     (if (real? pt)
         (let* ([span (- xmax xmin)]
                [delta : Real (/ span (max total 8))])
           (cond [(<= xmin pt xmax) (plot-mark-point-try-filter pt   delta     fx xmin xmax ymin ymax)]
                 [(< pt xmin)       (plot-mark-point-try-filter xmin delta     fx xmin xmax ymin ymax)]
                 [(> pt xmax)       (plot-mark-point-try-filter xmax (- delta) fx xmin xmax ymin ymax)]         
                 [else (plot-mark-point-try-filter (~lerp (+ (* span (max (min (car frac-rng) (cdr frac-rng)) 0.0)) xmin)
                                                          (+ (* span (min (max (car frac-rng) (cdr frac-rng)) 1.0)) xmin)
                                                          (cond [(and (>= frac 0.0) (<= frac 1.0)) frac]
                                                                [else (/ (max (- idx 1/2) 0) total)]))
                                                   delta fx xmin xmax ymin ymax)]))
         (let ([x (real-part pt)]
               [y (imag-part pt)])
           (cond [(<= xmin x xmax) (and (<= ymin y ymax) pt)]
                 [else (plot-mark-point-guard x fx idx total xmin xmax ymin ymax frac frac-rng)])))]
    [(θ-or-pt rθ idx total θmin θmax xmin xmax ymin ymax frac frac-rng)
     (if (real? θ-or-pt)
         (let* ([span (- θmax θmin)]
                [delta : Real (/ span (max total 8))])
           (cond [(<= θmin θ-or-pt θmax) (plot-mark-point-try-filter θ-or-pt delta     rθ θmin θmax xmin xmax ymin ymax)]
                 [(< θ-or-pt θmin)       (plot-mark-point-try-filter    θmin delta     rθ θmin θmax xmin xmax ymin ymax)]
                 [(> θ-or-pt θmax)       (plot-mark-point-try-filter    θmax (- delta) rθ θmin θmax xmin xmax ymin ymax)]         
                 [else (plot-mark-point-try-filter (~lerp (+ (* span (max (min (car frac-rng) (cdr frac-rng)) 0.0)) θmin)
                                                          (+ (* span (min (max (car frac-rng) (cdr frac-rng)) 1.0)) θmin)
                                                          (cond [(and (>= frac 0.0) (<= frac 1.0)) frac]
                                                                [else (/ (max (- idx 1/2) 0) total)]))
                                                   delta rθ θmin θmax xmin xmax ymin ymax)]))
         (let ([x (real-part θ-or-pt)]
               [y (imag-part θ-or-pt)])
           (cond [(<= xmin x xmax) (and (<= ymin y ymax) θ-or-pt)]
                 [else (plot-mark-point-guard +nan.0 rθ idx total θmin θmax xmin xmax ymin ymax frac frac-rng)])))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-mark-point-try-filter : (case-> [Real Real (-> Real Real) Real Real Real Real -> (Option Complex)]
                                             [Real Real (-> Real Real) Real Real Real Real Real Real -> (Option Complex)])
  (case-lambda
    [(x delta fx xmin xmax ymin ymax)
     (and (<= xmin x xmax)
          (let ([y : Real (fx x)])
            (cond [(<= ymin y ymax) (make-rectangular x y)]
                  [(zero? delta) #false]
                  [else (plot-mark-point-try-filter (+ x delta) delta fx xmin xmax ymin ymax)])))]
    [(θ delta rθ θmin θmax xmin xmax ymin ymax)
     (and (<= θmin θ θmax)
          (let* ([pt (make-polar (rθ θ) θ)]
                 [x (real-part pt)]
                 [y (imag-part pt)])
            (cond [(and (<= xmin x xmax) (<= ymin y ymax)) pt]
                  [(zero? delta) #false]
                  [else (plot-mark-point-try-filter (+ θ delta) delta rθ θmin θmax xmin xmax ymin ymax)])))]))

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
         (cond [(rational? maybe-angle) (cons length (real->double-flonum maybe-angle))]
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
(define plot-mark-point-guard : (-> Complex (-> Real Real) Positive-Fixnum Positive-Index Real Real Real Real Real (Pairof Real Real) (Option Complex))
  (lambda [pt x->dot idx total xmin xmax ymin ymax frac frac-rng]
    (if (real? pt)
        (let ([delta : Real (* (- xmax xmin) 1/8)])
          (cond [(<= xmin pt xmax) (plot-mark-point-try-filter pt delta x->dot xmin xmax ymin ymax)]
                [(< pt xmin) (plot-mark-point-try-filter xmin     delta x->dot xmin xmax ymin ymax)]
                [(> pt xmax) (plot-mark-point-try-filter xmax (- delta) x->dot xmin xmax ymin ymax)]         
                [else (let* ([span (- xmax xmin)]
                             [s (+ (* span (max (min (car frac-rng) (cdr frac-rng)) 0.0)) xmin)]
                             [e (+ (* span (min (max (car frac-rng) (cdr frac-rng)) 1.0)) xmin)]
                             [t (if (<= 0.0 frac 1.0) frac (/ (- idx 1/2) total))])
                        (plot-mark-point-try-filter (+ (* (- e s) t) s)
                                                    delta x->dot xmin xmax ymin ymax))]))
        (let ([x (real-part pt)]
              [y (imag-part pt)])
          (if (<= xmin x xmax)
              (and (<= ymin y ymax) pt)
              (plot-mark-point-guard x x->dot idx total xmin xmax ymin ymax
                                     frac frac-rng))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-mark-point-try-filter : (-> Real Real (-> Real Real) Real Real Real Real (Option Complex))
  (lambda [x delta x->dot xmin xmax ymin ymax]
    (and (<= xmin x xmax)
         (let ([y : Real (x->dot x)])
           (cond [(<= ymin y ymax) (make-rectangular x y)]
                 [(zero? delta) #false]
                 [else (plot-mark-point-try-filter (+ x delta) delta x->dot xmin xmax ymin ymax)])))))

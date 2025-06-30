#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-mark-point-guard : (-> Complex (-> Real Real) Positive-Fixnum Positive-Index Real Real Real Real Real (Option Complex))
  (lambda [pt x->dot idx total xmin xmax ymin ymax frac]
    (if (real? pt)
        (let ([delta : Real (* (- xmax xmin) 1/8)])
          (cond [(<= xmin pt xmax) (plot-mark-point-try-filter pt delta x->dot xmin xmax ymin ymax)]
                [(< pt xmin) (plot-mark-point-try-filter xmin     delta x->dot xmin xmax ymin ymax)]
                [(> pt xmax) (plot-mark-point-try-filter xmax (- delta) x->dot xmin xmax ymin ymax)]         
                [else (let ([t (if (< 0.0 frac 1.0) frac (/ (- idx 1/2) total))])
                        (plot-mark-point-try-filter (+ (* (- xmax xmin) t) xmin)
                                                    delta x->dot xmin xmax ymin ymax))]))
        (let ([x (real-part pt)]
              [y (imag-part pt)])
          (if (<= xmin x xmax)
              (and (<= ymin y ymax) pt)
              (plot-mark-point-guard x x->dot idx total xmin xmax ymin ymax frac))))))

(define plot-mark-polar-guard : (-> Complex (-> Real Real) Positive-Fixnum Positive-Index Real Real Real Real Real (Option Complex))
  (lambda [pt x->dot idx total xmin xmax ymin ymax frac]
    (if (real? pt)
        (let ([delta : Real (* (- xmax xmin) 1/8)])
          (cond [(<= xmin pt xmax) (plot-mark-point-try-filter pt delta x->dot xmin xmax ymin ymax)]
                [(< pt xmin) (plot-mark-point-try-filter xmin     delta x->dot xmin xmax ymin ymax)]
                [(> pt xmax) (plot-mark-point-try-filter xmax (- delta) x->dot xmin xmax ymin ymax)]         
                [else (let ([t (if (< 0.0 frac 1.0) frac (/ (- idx 1/2) total))])
                        (plot-mark-point-try-filter (+ (* (- xmax xmin) t) xmin)
                                                    delta x->dot xmin xmax ymin ymax))]))
        (let ([x (real-part pt)]
              [y (imag-part pt)])
          (if (<= xmin x xmax)
              (and (<= ymin y ymax) pt)
              (plot-mark-point-guard x x->dot idx total xmin xmax ymin ymax frac))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-mark-point-try-filter : (-> Real Real (-> Real Real) Real Real Real Real (Option Complex))
  (lambda [x delta x->dot xmin xmax ymin ymax]
    (and (<= xmin x xmax)
         (let ([y : Real (x->dot x)])
           (cond [(<= ymin y ymax) (make-rectangular x y)]
                 [(zero? delta) #false]
                 [else (plot-mark-point-try-filter (+ x delta) delta x->dot xmin xmax ymin ymax)])))))

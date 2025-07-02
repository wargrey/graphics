#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Plot-Mark-Vector (U (Pairof Flonum Flonum) (List Flonum Flonum (U '% ':))))

(define-type Plot-Mark-Fallback-Angle (U Real (-> Real (Option Real))))
(define-type Plot-Mark-Fallback-Vector
  (U (Pairof Flonum Plot-Mark-Fallback-Angle)
     (List Flonum Plot-Mark-Fallback-Angle (U '% ':))))

(define plot-mark-null-vector : Plot-Mark-Vector (cons +nan.0 +nan.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-mark-vector : (-> Real+% Real Plot-Mark-Vector)
  (lambda [length angle]
    (define l-ok? (if (list? length) (rational? (car length)) (rational? length)))
    (define a-ok? (rational? angle))
    
    (if (list? length)
        (list (if (not l-ok?) +nan.0 (real->double-flonum (car length)))
              (if (not a-ok?) +nan.0 (real->double-flonum angle))
              (cadr length))
        (cons (if (not l-ok?) +nan.0 (real->double-flonum length))
              (if (not a-ok?) +nan.0 (real->double-flonum angle))))))

(define plot-mark-pin-vector : (-> Boolean (Option Real+%) (Option Real) (Option Plot-Mark-Vector))
  (lambda [pin? length angle]
    (and pin?
         (or length angle)
         (plot-mark-vector (or length +nan.0)
                           (or  angle +nan.0)))))

(define plot-mark-fallback-vector-guard : (-> Real+% Real Plot-Mark-Fallback-Angle (Option Plot-Mark-Fallback-Vector))
  (lambda [length maybe-angle dynamic-angle]
    (and (if (list? length) (rational? (car length)) (rational? length))
         (let ([angle (cond [(rational? maybe-angle) (real->double-flonum maybe-angle)]
                            [(procedure? dynamic-angle) dynamic-angle]
                            [(rational? dynamic-angle) dynamic-angle]
                            [else #false])])
           (and angle
                (if (list? length)
                    (list (real->double-flonum (car length)) angle (cadr length))
                    (cons (real->double-flonum length) angle)))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-mark-vector-guard : (-> (Option Plot-Mark-Vector) (Option Plot-Mark-Fallback-Vector) Nonnegative-Flonum Real
                                     (Option Float-Complex))
  (lambda [self fallback-vctr 100% x]
    (define-values (length angle)
      (cond [(list? fallback-vctr)
             (values (list (car fallback-vctr) (caddr fallback-vctr))
                     (cadr fallback-vctr))]
            [(pair? fallback-vctr)
             (values (car fallback-vctr) (cdr fallback-vctr))]
            [else (values +nan.0 +nan.0)]))

    (define (guard [guarded-len : Flonum] [rad : Flonum]) : (Option Float-Complex)
      (define guarded-rad
        (cond [(rational? rad) rad]
              [(procedure? angle) (real->double-flonum (or (angle x) +nan.0))]
              [else (real->double-flonum angle)]))

      (and (rational? guarded-len)
           (rational? guarded-rad)
           (make-polar guarded-len guarded-rad)))
    
    (cond [(list? self)
           (let* ([len (car self)]
                  [len (if (rational? len) (list len (caddr self)) length)])
             (guard (~length len 100%)
                    (cadr self)))]
          [(pair? self)
           (let ([len (car self)])
             (guard (if (rational? len) len (~length length 100%))
                    (cdr self)))]
          [else #false])))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-mark-point-try-filter : (-> Real Real (-> Real Real) Real Real Real Real (Option Complex))
  (lambda [x delta x->dot xmin xmax ymin ymax]
    (and (<= xmin x xmax)
         (let ([y : Real (x->dot x)])
           (cond [(<= ymin y ymax) (make-rectangular x y)]
                 [(zero? delta) #false]
                 [else (plot-mark-point-try-filter (+ x delta) delta x->dot xmin xmax ymin ymax)])))))

#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-mark-vector : (-> Real Real Float-Complex)
  (lambda [angle length]
    (define a-ok? (rational? angle))
    (define l-ok? (and (rational? length)
                       (not (zero? length))))
    
    (if (and a-ok? l-ok?)
        (make-polar (real->double-flonum length) (real->double-flonum angle))
        (make-rectangular (if (not l-ok?) +nan.0 (real->double-flonum length))
                          (if (not a-ok?) +nan.0 (real->double-flonum angle))))))

(define plot-mark-pin-vector : (-> Boolean (Option Real) (Option Real) (Option Float-Complex))
  (lambda [pin? angle length]
    (and (and pin? (or angle length))
         (plot-mark-vector (or angle  +nan.0)
                           (or length +nan.0)))))

(define plot-mark-vector-okay? : (-> Float-Complex Boolean)
  (lambda [self]
    (define len (real-part self))
    (define rad (imag-part self))

    (and (rational? rad)
         (rational? len)
         (not (zero? len)))))

(define plot-mark-vector-ensure : (case-> [Float-Complex Flonum Flonum -> Float-Complex]
                                          [(Option Float-Complex) Flonum Flonum -> (Option Float-Complex)])
  (lambda [self length angle]
    (displayln self)
    (and self
         (let ([len (real-part self)]
               [rad (imag-part self)])
           (define a-ok? (rational? rad))
           (define l-ok? (and (rational? len)
                              (not (zero? len))))
           
           (cond [(and a-ok? l-ok?) self]
                 [else (make-polar (if (not l-ok?) length len)
                                   (if (not a-ok?) angle  rad))])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-mark-point-filter : (-> Complex (-> Real Real) Positive-Fixnum Positive-Index Real Real Real Real Real (Option Complex))
  (lambda [pt x->dot idx total xmin xmax ymin ymax frac]
    (if (real? pt)
        (cond [(<= xmin pt xmax) (plot-mark-point-filter (make-rectangular pt (x->dot pt)) x->dot idx total xmin xmax ymin ymax frac)]
              [(< pt xmin) (plot-mark-point-filter xmin x->dot idx total xmin xmax ymin ymax frac)]
              [(> pt xmax) (plot-mark-point-filter xmax x->dot idx total xmin xmax ymin ymax frac)]         
              [else (let ([t (if (rational? frac) frac (/ (- idx 1/2) total))])
                      (plot-mark-point-filter (+ (* (- xmax xmin) t) xmin) x->dot idx total xmin xmax ymin ymax frac))])
        (let ([x (real-part pt)]
              [y (imag-part pt)])
          (if (<= xmin x xmax)
              (and (<= ymin y ymax) pt)
              (plot-mark-point-filter x x->dot idx total xmin xmax ymin ymax frac))))))

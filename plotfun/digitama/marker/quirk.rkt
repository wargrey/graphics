#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-mark-vector : (-> Real+% Real FlComplex+%)
  (lambda [length angle]
    (define l-ok? (if (list? length) (rational? (car length)) (rational? length)))
    (define a-ok? (rational? angle))
    
    (cond [(list? length)
           (list (make-rectangular (if (not l-ok?) +nan.0 (real->double-flonum (car length)))
                                   (if (not a-ok?) +nan.0 (real->double-flonum angle)))
                 (cadr length))]
          [(not (and a-ok? l-ok?))
           (make-rectangular (if (not l-ok?) +nan.0 (real->double-flonum length))
                             (if (not a-ok?) +nan.0 (real->double-flonum angle)))]
          [else (make-polar (real->double-flonum length) (real->double-flonum angle))])))

(define plot-mark-pin-vector : (-> Boolean (Option Real+%) (Option Real) (Option FlComplex+%))
  (lambda [pin? length angle]
    (and pin?
         (or length angle)
         (plot-mark-vector (or length +nan.0)
                           (or  angle +nan.0)))))

(define plot-mark-vector-okay? : (-> FlComplex+% Boolean)
  (lambda [self]
    (cond [(list? self) (plot-mark-vector-okay? (car self))]
          [else (let ([len (real-part self)]
                      [rad (imag-part self)])
                  (and (rational? rad)
                       (rational? len)))])))

(define plot-mark-vector-ensure : (case-> [FlComplex+% Complex+% Nonnegative-Flonum -> Float-Complex]
                                          [(Option FlComplex+%) Complex+% Nonnegative-Flonum -> (Option Float-Complex)])
  (lambda [self vctr 100%]
    (define-values (length theta)
      (if (list? vctr)
          (values (list (magnitude (car vctr)) (cadr vctr))
                  (angle (car vctr)))
          (values (magnitude vctr) (angle vctr))))
    
    (cond [(complex? self)
           (let ([len (real-part self)]
                 [rad (imag-part self)])
             (define l-ok? (rational? len))
             (define a-ok? (rational? rad))
             
             (cond [(and a-ok? l-ok?) self]
                   [else (make-polar (if (not l-ok?)       (~length length 100%) len)
                                     (if (not a-ok?) (real->double-flonum theta) rad))]))]
          [(list? self)
           (let ([len (real-part (car self))]
                 [rad (imag-part (car self))])
             (define l-ok? (rational? len))
             (define a-ok? (rational? rad))

             (make-polar (~length (if (not l-ok?) length (list len (cadr self))) 100%)
                         (if (not a-ok?) (real->double-flonum theta) rad)))]
          [else #false])))

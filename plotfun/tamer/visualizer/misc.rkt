#lang typed/racket/base

(require plotfun/cartesian)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define |Σ1/(x+n)| : (-> Real (Option Real))
  (lambda [x]
    (+ (/ 1 (+ x 1))
       (/ 1 (+ x 2))
       (/ 1 (+ x 3)))))

(define x^3-3x^2 : (-> Real (Option Real))
  (lambda [x]
    (- (* x x x)
       (* 3 x x))))

(define bineq-f : (-> Positive-Byte (-> Real (Option Real)))
  (lambda [co]
    (procedure-rename
     (λ [[x : Real]]
       (and (not (zero? (floor (* (abs (- x 1/2)) 100.0))))
            (+ (/ 1 (- (* 2 x) 1))
               (/ co (- 2 x)))))
     (string->symbol (format "~a/b" co)))))

(define (|f(x)=f(x-2)| [x : Real]) : Real
  (if (< x 0)
      (- (+ (* x x) (* 2 x) 0))
      (|f(x)=f(x-2)| (- x 2))))

(define (|±x²+2x+1| [x : Real]) : Real
  (abs (if (>= x 0)
           (+ (* x x +1) (* 2 x) 1)
           (+ (* x x -1) (* 2 x) 1))))

(define (|x⁴-2x²+2| [x : Real]) : Real
  (+ (* x x x x) (* -2 x x) 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(plot-cartesian
 #:x-range (cons -8 6)
 #:y-range (cons -80 80)
 #:height 400.0
 (function |Σ1/(x+n)|   -7 3)
 (function x^3-3x^2 -4 6)
 (function (bineq-f 32))
 (function (bineq-f  2) 1/2 2))

(plot-cartesian
 #:width 800.0
 #:height 800.0
 #:x-range (cons -3 6)
 (list (function |f(x)=f(x-2)|)))

(plot-cartesian
 #:width 400.0
 #:height 400.0
 #:x-range (cons -2 2)
 #:y-range (cons 0 9)
 (list (function |±x²+2x+1|)
       (function |x⁴-2x²+2|)))

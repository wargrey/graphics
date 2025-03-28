#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define scaled-round : (->* (Flonum) (Nonnegative-Flonum) Flonum)
  (lambda [fl [precision 10000.0]]
    (round (* fl precision))))

(define exact-log : (-> Positive-Real Exact-Rational)
  (lambda [x]
    (define y (log x))
    (if (infinite? y)
        (- (inexact->exact (log (numerator x)))
           (inexact->exact (log (denominator x))))
        (inexact->exact y))))

(define floor-log : (->* (Positive-Real) (Positive-Integer) Integer)
  (lambda [x [b 10]]
    (define q (inexact->exact x))
    (define m (floor (/ (exact-log q) (inexact->exact (log b)))))

    (let loop ([m m]
               [p  (expt b m)])
      (if (>= q p)
          (let ([u (* p b)])
            (cond [(>= q u)  (loop (add1 m) u)]
                  [else  m]))
          (loop (sub1 m) (/ p b))))))

(define ceiling-log : (->* (Positive-Real) (Positive-Integer) Integer)
  (lambda [x [b 10]]
    (- (floor-log (/ (inexact->exact x)) b))))

(define magnitude-order : (->* (Real) (Positive-Integer) Positive-Exact-Rational)
  (lambda [x [base 10]]
    (if (positive? x)
        (expt base (floor-log x base))
        (raise-argument-error 'magnitude-order "positive?" x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tick-precision : (->* (Real Real) (Positive-Integer Integer) Integer)
  (lambda [x-min x-max [base 10] [extra-digits 3]]
    (define range (abs (- x-max x-min)))
    (+ extra-digits
       (cond [(zero? range) 0]
             [else (- (floor-log range base))]))))

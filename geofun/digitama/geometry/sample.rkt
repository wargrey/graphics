#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)
(require racket/math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ~cartesian2ds : (-> (-> Flonum (Option Flonum)) (Listof Real) Flonum Flonum
                            Flonum Flonum Flonum Flonum (-> Flonum Flonum Float-Complex)
                            (Values (Listof Float-Complex) Flonum Flonum Flonum Flonum))
  (lambda [f xs ymin ymax dx dy sx sy transform]
    (let normalize ([xs : (Listof Real) xs]
                    [stod : (Listof Float-Complex) null]
                    [lx : Flonum +inf.0]
                    [ty : Flonum +inf.0]
                    [rx : Flonum -inf.0]
                    [by : Flonum -inf.0])
      (cond [(pair? xs)
             (let*-values ([(x0 rest) (values (real->double-flonum (car xs)) (cdr xs))]
                           [(y0) (or (f x0) +nan.0)]
                           [(x y) (values (+ (* x0 sx) dx) (and y0 (+ (* y0 sy) dy)))])
               (if (not (<= ymin y0 ymax))
                   (normalize rest (cons +nan.0+nan.0i stod) lx ty rx by)
                   (let*-values ([(dot) (transform x y)]
                                 [(scr-x scr-y) (values (real-part dot) (imag-part dot))])
                     (normalize rest (cons dot stod)
                                (min lx scr-x) (min ty scr-y)
                                (max rx scr-x) (max by scr-y)))))]
            [else (values (reverse stod) lx ty rx by)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-linear-samples : (->* (Real Real Positive-Index) (#:start? Boolean #:end? Boolean) (Listof Real))
  (lambda [start end num #:start? [start? #true] #:end? [end? #true]]
    (cond [(and start? end? (= 1 num))  (list (real->double-flonum start))]
          [(< end start) (geo-linear-samples end start num #:start? end? #:end? start?)]
          [(= end start) (make-list num (real->double-flonum start))]
          [else (let ([size (- end start)])
                  (define step : Real
                    (/ size (cond [(and start? end?)  (- num 1)]
                                  [(or start? end?)   (- num 1/2)]
                                  [else               num])))
                  (define real-start
                    (cond [start?  start]
                          [else    (+ start (* 0.5 step))]))
                  
                  (geo-linear-samples-generate real-start step num))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-linear-samples-generate : (->* (Real Real Index) (Real Real) (Listof Real))
  (lambda [start step num [min-val start] [max-val (+ start (* (- num 1) step))]]
    (define n-start (max 0 (inexact->exact (floor (/ (- min-val start) step)))))
    (define n-end (min num (+ (inexact->exact (ceiling (/ (- max-val start) step))) 1)))

    (for*/list : (Listof Real) ([n (in-range n-start n-end)]
                                [x (in-value (+ start (* n step)))]
                                #:when (<= min-val x max-val))
      x)))

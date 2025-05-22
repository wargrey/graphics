#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ~y-bounds : (-> (-> Real (Option Number)) (Listof Real) (Values Real Real))
  (lambda [f xs]
    (for/fold ([ymin : Real +inf.0]
               [ymax : Real -inf.0])
              ([x (in-list xs)])
      (define y (f x))
      (if (rational? y)
          (values (min ymin y) (max ymax y))
          (values ymin         ymax)))))

(define ~cartesian2ds : (-> (-> Real (Option Number)) (Listof Real) Real Real (-> Flonum Flonum Float-Complex)
                            (Values (Listof Float-Complex) Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [f xs ymin ymax transform]
    (define-values (flmin flmax) (values (real->double-flonum ymin) (real->double-flonum ymax)))
    (let normalize ([xs : (Listof Real) xs]
                    [stod : (Listof Float-Complex) null]
                    [lx : Flonum +inf.0]
                    [ty : Flonum +inf.0]
                    [rx : Flonum -inf.0]
                    [by : Flonum -inf.0])
      (cond [(pair? xs)
             (let*-values ([(flx rest) (values (real->double-flonum (car xs)) (cdr xs))]
                           [(y) (f flx)]
                           [(fly) (if (rational? y) (real->double-flonum y) +nan.0)])
               (cond [(<= flmin fly flmax)
                      (let*-values ([(dot) (transform flx fly)]
                                    [(scr-x scr-y) (values (real-part dot) (imag-part dot))])
                        (normalize rest (cons dot stod)
                                   (min lx scr-x) (min ty scr-y)
                                   (max rx scr-x) (max by scr-y)))]
                     [(and (pair? stod) (eq? (car stod) +nan.0+nan.0i)) (normalize rest stod lx ty rx by)]
                     [else (normalize rest (cons +nan.0+nan.0i stod) lx ty rx by)]))]
            [(rational? lx) (values (reverse stod) lx ty (max (- rx lx) 0.0) (max (- by ty) 0.0))]
            [else (values (reverse stod) 0.0 0.0 0.0 0.0)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-linear-samples : (->* (Real Real Positive-Index) (#:start? Boolean #:end? Boolean) (Listof Real))
  (let ([sample-db : (Weak-HashTable Any (Listof Real)) (make-weak-hash)])
    (lambda [start end num #:start? [start? #true] #:end? [end? #true]]
      (cond [(and start? end? (= 1 num))  (list start)]
            [(< end start) (geo-linear-samples end start num #:start? end? #:end? start?)]
            [else (hash-ref! sample-db (list start end num start? end?)
                             (λ [] (let ([size (- end start)])
                                     (define step : Real
                                       (/ size (cond [(and start? end?)  (- num 1)]
                                                     [(or start? end?)   (- num 1/2)]
                                                     [else               num])))
                                     (define real-start
                                       (cond [start?  start]
                                             [else    (+ start (* 1/2 step))]))

                                     (if (= size 0)
                                         (make-list num start)
                                         (geo-linear-samples-generate real-start step num)))))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-linear-samples-generate : (->* (Real Real Index) (Real Real) (Listof Real))
  (lambda [start step num [min-val start] [max-val (+ start (* (- num 1) step))]]
    (define n-start (max 0 (inexact->exact (floor (/ (- min-val start) step)))))
    (define n-end (min num (+ (inexact->exact (ceiling (/ (- max-val start) step))) 1)))
    (define precision 10000.0)

    (for*/list : (Listof Real) ([n (in-range n-start n-end)]
                                [x (in-value (+ start (* n step)))]
                                #:when (<= min-val x max-val))
      x)))

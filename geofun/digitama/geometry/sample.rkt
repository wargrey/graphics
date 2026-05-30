#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)
(require racket/math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-sample-count : (case-> [Real Real Index -> Positive-Index]
                                   [Real Real (-> Flonum Flonum Float-Complex) Index -> Positive-Index])
  (case-lambda
    [(xmin xmax density)
     (let ([count (exact-ceiling (* (abs (- xmax xmin)) (if (> density 0) density 2)))])
       (max (assert count index?) 1))]
    [(xmin xmax transform density)
     (let* ([vmin (real-part (transform (real->double-flonum xmin) 0.0))]
            [vmax (real-part (transform (real->double-flonum xmax) 0.0))])
       (define fldensity
         (cond [(> density 0) (exact->inexact density)]
               [else (let ([y/x (/ (abs (imag-part (- (transform 0.0 1.0) (transform 0.0 0.0))))
                                   (abs (real-part (- (transform 1.0 0.0) (transform 0.0 0.0)))))])
                       (* 2.0 (max y/x 1.0)))]))

       (let ([count (exact-ceiling (* (abs (- vmax vmin)) fldensity))])
         (max (assert count index?) 1)))]))

(define geo-linear-samples : (->* (Real Real Positive-Index)
                                  (#:start? Boolean #:end? Boolean #:fixed-samples (Listof Real))
                                  (Listof Real))
  (let ([sample-db : (Weak-HashTable Any (Listof Real)) (make-weak-hash)])
    (lambda [start end num #:start? [start? #true] #:end? [end? #true] #:fixed-samples [fixed null]]
      (cond [(and start? end? (= 1 num))  (list start)]
            [(< end start) (geo-linear-samples end start num #:start? end? #:end? start? #:fixed-samples fixed)]
            [else (let ([sorted-fixed-samples (sort (remove-duplicates fixed) <)])
                    (hash-ref! sample-db (list start end num start? end? sorted-fixed-samples)
                               (λ [] (let ([size (- end start)])
                                       (define step : Real
                                         (/ size (cond [(and start? end?) (- num 1)]
                                                       [(or start? end?)  (- num 1/2)]
                                                       [else              num])))
                                       (define real-start
                                         (cond [start?  start]
                                               [else    (+ start (* 1/2 step))]))
                                       
                                       (if (= size 0)
                                           (make-list num start)
                                           (geo-linear-samples-generate real-start step num
                                                                        sorted-fixed-samples))))))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-linear-samples-generate : (->* (Real Real Index (Listof Real)) (Real Real) (Listof Real))
  (lambda [start step num fixed [min-val start] [max-val (+ start (* (- num 1) step))]]
    (define n-start (max 0 (inexact->exact (floor (/ (- min-val start) step)))))
    (define n-end (min num (+ (inexact->exact (ceiling (/ (- max-val start) step))) 1)))

    (if (null? fixed)
        (for*/list : (Listof Real) ([n (in-range n-start n-end)]
                                    [x (in-value (+ start (* n step)))]
                                    #:when (<= min-val x max-val))
          x)
        
        (let gen ([n : Nonnegative-Exact-Rational n-start]
                  [fixed : (Listof Real) fixed]
                  [samples : (Listof Real) null])
          (define x (+ start (* n step)))
          
          (cond [(> n n-end) (reverse samples)]
                [(<= min-val x max-val)
                 (if (pair? fixed)
                     (if (>= x (car fixed))
                         (gen n (cdr fixed) (cons (car fixed) samples))
                         (gen (+ n 1) fixed (cons x samples)))
                     (gen (+ n 1) fixed (cons x samples)))]
                [else (gen (+ n 1) fixed samples)])))))

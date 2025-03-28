#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; for the sake of simplicity, removed `(List Complex Any)`
(define-type Plot-Axis-Real-Datum (U Complex (Pairof Complex Any)))
(define-type Plot-Axis-Integer-Datum (U Integer (Pairof Integer Any)))

(define plot-axis-real-values : (-> Plot-Axis-Real-Datum (Values Flonum Any))
  (lambda [r]
    (cond [(complex? r) (values (real->double-flonum (real-part r)) r)]
          [else (values (real->double-flonum (real-part (car r))) (cdr r))])))

(define plot-axis-integer-values : (-> Plot-Axis-Integer-Datum (Values Integer Any))
  (lambda [r]
    (cond [(exact-integer? r) (values r r)]
          [else (values (car r) (cdr r))])))

(define plot-axis-real-value : (-> Plot-Axis-Real-Datum Real)
  (lambda [r]
    (cond [(complex? r) (real-part r)]
          [else (real-part (car r))])))

(define plot-axis-integer-value : (-> Plot-Axis-Integer-Datum Integer)
  (lambda [r]
    (cond [(exact-integer? r) r]
          [else (car r)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-axis-reals-from-vector : (->* ((U (Listof Any) (Vectorof Any))) (Index) (Listof (Pairof Index Any)))
  (lambda [vs [base 0]]
    (for/list : (Listof (Pairof Index Any)) ([v (if (list? vs) (in-list vs) (in-vector vs))]
                                             [i (in-naturals base)]
                                             #:when (index? i))
      (cons i v))))

(define #:forall (R) plot-axis-reals-from-producer : (-> (-> R Any) (Listof R) (Listof (Pairof R Any)))
  (lambda [f xs]
    (for/list : (Listof (Pairof R Any)) ([x (in-list xs)])
      (cons x (f x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-axis-real-range : (-> (U (Listof Plot-Axis-Real-Datum) Procedure) (Option (Pairof Real Real)))
  (lambda [reals]
    (and (pair? reals)
         (let*-values ([(1st) (plot-axis-real-value (car reals))]
                       [(2nd rest) (if (null? (cdr reals)) (values 1st null) (values (plot-axis-real-value (cadr reals)) (cddr reals)))])
           (let pseudo-sort ([left  (min 1st 2nd)]
                             [right (max 1st 2nd)]
                             [rest rest])
             (cond [(pair? rest)
                    (let-values ([(head tail) (values (plot-axis-real-value (car rest)) (cdr rest))])
                      (pseudo-sort (min left head) (max right head) tail))]
                   [(not (= left right)) (cons left right)]
                   [(negative? left) (cons left 0)]
                   [(positive? left) (cons 0 left)]
                   [else #false]))))))

(define plot-axis-integer-range : (case-> [(U (Listof Plot-Axis-Integer-Datum) Procedure) -> (Option (Pairof Integer Integer))]
                                          [(U (Listof Plot-Axis-Integer-Datum) (Vectorof Any) Procedure) Boolean -> (Option (Pairof Integer Integer))])
  (case-lambda
    [(reals exclude-zero?)
     (if (vector? reals)
         (let ([size (vector-length reals)])
           (cond [(= size 0) #false]
                 [(not exclude-zero?) (cons 0 size)]
                 [else (cons 1 (add1 size))]))
         (plot-axis-integer-range reals))]
    [(reals)
     (and (pair? reals)
          (let*-values ([(1st) (plot-axis-integer-value (car reals))]
                        [(2nd rest) (if (null? (cdr reals)) (values 1st null) (values (plot-axis-integer-value (cadr reals)) (cddr reals)))])
            (let pseudo-sort ([left  (min 1st 2nd)]
                              [right (max 1st 2nd)]
                              [rest rest])
              (cond [(pair? rest)
                     (let-values ([(head tail) (values (plot-axis-integer-value (car rest)) (cdr rest))])
                       (pseudo-sort (min left head) (max right head) tail))]
                    [(not (= left right)) (cons left right)]
                    [(negative? left) (cons left 0)]
                    [(positive? left) (cons 0 left)]
                    [else #false]))))]))

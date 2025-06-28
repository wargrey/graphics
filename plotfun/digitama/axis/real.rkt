#lang typed/racket/base

(provide (all-defined-out))

(require "../marker/self.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Plot-Axis-Real-Datum (U Complex Plot:Mark))
(define-type Plot-Axis-Integer-Datum (U Integer Plot:Mark))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-axis-list->marks : (-> (Listof (U Plot-Axis-Real-Datum Plot-Axis-Integer-Datum)) (U Plot-Mark->Description Plot:Mark) (Listof Plot:Mark))
  (lambda [vs desc]
    (for/list : (Listof Plot:Mark) ([v (in-list vs)])
      (if (complex? v)
          (if (plot:mark? desc)
              (remake-plot:mark desc #:point (real-part v) #:datum v)
              (plot-real (real-part v) #:desc desc #:datum v))
          (cond [(plot:mark? desc) (plot-mark-sync-with-template v desc)]
                [(eq? (plot:mark-desc v) desc) v]
                [else (remake-plot:mark v #:desc desc)])))))

(define plot-axis-vector->marks : (->* ((U (Listof Any) (Vectorof Any)) (U Plot-Mark->Description Plot:Mark)) (Index) (Listof Plot:Mark))
  (lambda [vs desc [base 0]]
    (for/list : (Listof Plot:Mark) ([v (if (list? vs) (in-list vs) (in-vector vs))]
                                    [i (in-naturals base)])
      (if (plot:mark? desc)
          (remake-plot:mark desc #:point i #:datum v)
          (plot-real i #:desc desc #:datum v)))))

(define #:forall (R) plot-axis-produce-marks : (-> (-> R Any) (Listof (∩ R Real)) (U Plot-Mark->Description Plot:Mark) (Listof Plot:Mark))
  (lambda [f xs desc]
    (for/list : (Listof Plot:Mark) ([x (in-list xs)])
      (if (plot:mark? desc)
          (remake-plot:mark desc #:point x #:datum (f x))
          (plot-real x #:desc desc #:datum (f x))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-axis-real-value : (-> Plot-Axis-Real-Datum Real)
  (lambda [r]
    (cond [(complex? r) (real-part r)]
          [else (real-part (plot:mark-point r))])))

(define plot-axis-integer-value : (-> Plot-Axis-Integer-Datum Integer)
  (lambda [r]
    (cond [(exact-integer? r) r]
          [else (let ([pt (plot:mark-point r)])
                  (cond [(exact-integer? pt) pt]
                        [else (raise-argument-error 'plot-integer-axis "integer?" pt)]))])))

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

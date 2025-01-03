#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/dc/composite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct plot:axis geo:group
  ([origin : Float-Complex]
   [tick-digits : (Listof Integer)])
  #:type-name Plot:Axis
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Plot-Axis-Real-Datum (U Complex (Pairof Complex Any) (List Complex Any)))

(define plot-axis-real-values : (-> Plot-Axis-Real-Datum (Values Flonum Any))
  (lambda [r]
    (cond [(complex? r) (values (real->double-flonum (real-part r)) r)]
          [(list? r) (values (real->double-flonum (real-part (car r))) (cadr r))]
          [else (values (real->double-flonum (real-part (car r))) (cdr r))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-axis-reals-from-vector : (-> (Vectorof Any) (Listof (Pairof Index Any)))
  (lambda [vs]
    (for/list : (Listof (Pairof Index Any)) ([v (in-vector vs)]
                                             [i (in-naturals 0)]
                                             #:when (index? i))
      (cons i v))))

(define plot-axis-reals-from-producer : (-> (-> Integer Any) (Listof Integer) (Listof (Pairof Integer Any)))
  (lambda [f xs]
    (for/list : (Listof (Pairof Integer Any)) ([x (in-list xs)])
      (cons x (f x)))))

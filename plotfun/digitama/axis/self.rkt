#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/dc/composite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Plot-Axis-Position-Map (-> Flonum Float-Complex))

(define-type Plot-Cartesian-Position-Map
  (case-> [Flonum Flonum -> Float-Complex]
          [Float-Complex -> Float-Complex]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct plot:axis geo:group
  ([origin : Float-Complex]
   [tick-digits : (Listof Integer)]
   [map : Plot-Axis-Position-Map])
  #:type-name Plot:Axis
  #:transparent)

(struct plot:cartesian geo:group
  ([origin : Float-Complex]
   [xtick-digits : (Listof Integer)]
   [ytick-digits : (Listof Integer)]
   [map : Plot-Cartesian-Position-Map])
  #:type-name Plot:Cartesian
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; for the sake of simplicity, removed `(List Complex Any)`
(define-type Plot-Axis-Real-Datum (U Complex (Pairof Complex Any)))

(define plot-axis-real-values : (-> Plot-Axis-Real-Datum (Values Flonum Any))
  (lambda [r]
    (cond [(complex? r) (values (real->double-flonum (real-part r)) r)]
          [else (values (real->double-flonum (real-part (car r))) (cdr r))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-axis-reals-from-vector : (->* ((Vectorof Any)) (Index) (Listof (Pairof Index Any)))
  (lambda [vs [base 0]]
    (for/list : (Listof (Pairof Index Any)) ([v (in-vector vs)]
                                             [i (in-naturals base)]
                                             #:when (index? i))
      (cons i v))))

(define plot-axis-reals-from-producer : (-> (-> Integer Any) (Listof Integer) (Listof (Pairof Integer Any)))
  (lambda [f xs]
    (for/list : (Listof (Pairof Integer Any)) ([x (in-list xs)])
      (cons x (f x)))))

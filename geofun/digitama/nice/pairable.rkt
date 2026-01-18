#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Pairable-Datum (U Number Length+% Pairable:Datum))
(define-type (Pairable T)
  (U (∩ T Pairable-Datum)
     (Pairof (∩ T Pairable-Datum) (∩ T Pairable-Datum))
     (List (∩ T Pairable-Datum) (∩ T Pairable-Datum))))

(define-type Pairable-Real (U (Pairable Real) Complex))

(struct pairable:datum () #:type-name Pairable:Datum #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (P D) pairable-values : (case-> [(Pairable P) -> (Values P P)]
                                                 [(Pairable P) (-> P D) -> (Values D D)])
  (case-lambda
    [(self)
     (cond [(list? self) (values (car self) (cadr self))]
           [(pair? self) (values (car self) (cdr self))]
           [else (values self self)])]
    [(self ->datum)
     (cond [(list? self) (values (->datum (car self)) (->datum (cadr self)))]
           [(pair? self) (values (->datum (car self)) (->datum (cdr self)))]
           [else (let ([datum (->datum self)]) (values datum datum))])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define 2d-scale-values : (-> Pairable-Real (Values Flonum Flonum))
  (lambda [st]
    (define-values (sx0 sy0)
      (if (complex? st)
          (values (real->double-flonum (real-part st))
                  (real->double-flonum (imag-part st)))
          (pairable-values st real->double-flonum)))
    (define sx (if (and (rational? sx0) (not (= sx0 0.0))) sx0 1.0))
    (define sy (if (and (rational? sy0) (not (= sy0 0.0))) sy0  sx))

    (values sx sy)))

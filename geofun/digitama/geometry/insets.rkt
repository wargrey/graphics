#lang typed/racket/base

(provide (all-defined-out))

(require digimon/sequence)
(require digimon/measure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Insets-Datum (U Nonnegative-Real (Listof Nonnegative-Real) (Immutable-Vectorof Nonnegative-Real) Geo-Standard-Insets))
(define-type Geo-Insets-Datum+% (U Length+% (Listof Length+%) (Immutable-Vectorof Length+%) Geo-Standard-Insets))

(struct geo-standard-insets
  ([top : Nonnegative-Flonum]
   [right : Nonnegative-Flonum]
   [bottom : Nonnegative-Flonum]
   [left : Nonnegative-Flonum])
  #:type-name Geo-Standard-Insets
  #:transparent)

(define geo-insets : (case-> [Real -> Geo-Standard-Insets]
                             [Real Real -> Geo-Standard-Insets]
                             [Real Real Real -> Geo-Standard-Insets]
                             [Real Real Real Real -> Geo-Standard-Insets])
  (case-lambda
    [(val) (geo-insets*->insets val 100.0)]
    [(v h) (geo-insets*->insets (vector-immutable v h v h) 100.0)]
    [(t h b) (geo-insets*->insets (vector-immutable t h b h) 100.0)]
    [(t r b l) (geo-insets*->insets (vector-immutable t r b l) 100.0)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-inset-values : (-> (Option Geo-Insets-Datum)
                               (Values Nonnegative-Flonum Nonnegative-Flonum
                                       Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [self]
    (cond [(geo-standard-insets? self)
           (values (geo-standard-insets-top self)
                   (geo-standard-insets-right self)
                   (geo-standard-insets-bottom self)
                   (geo-standard-insets-left self))]
          [(vector? self) (vector->4:values self 0.0 real->double-flonum)]
          [(list? self) (list->4:values self 0.0 real->double-flonum)]
          [(real? self) (let ([fl (real->double-flonum self)]) (values fl fl fl fl))]
          [else (values 0.0 0.0 0.0 0.0)])))

(define geo-inset*-values : (-> (Option Geo-Insets-Datum+%) Nonnegative-Flonum
                                 (Values Nonnegative-Flonum Nonnegative-Flonum
                                         Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [self 100%]
    (define-values (top right bottom left)
      (cond [(vector? self) (vector->4:values self 0.0)]
            [(list? self) (list->4:values self 0.0)]
            [(length+%? self) (values self self self self)]
            [(geo-standard-insets? self)
             (values (geo-standard-insets-top self)
                     (geo-standard-insets-right self)
                     (geo-standard-insets-bottom self)
                     (geo-standard-insets-left self))]
            [else (values 0.0 0.0 0.0 0.0)]))
    (values (~dimension top 100%) (~dimension right 100%) (~dimension bottom 100%) (~dimension left 100%))))

(define geo-insets*->insets : (-> (Option Geo-Insets-Datum+%) Nonnegative-Flonum Geo-Standard-Insets)
  (lambda [self 100%]
    (define-values (top right bottom left) (geo-inset*-values self 100%))
    
    (geo-standard-insets (~dimension top 100%) (~dimension right 100%)
                         (~dimension bottom 100%) (~dimension left 100%))))

(define geo-insets-scale : (case-> [Geo-Standard-Insets Real -> Geo-Standard-Insets]
                                   [Geo-Standard-Insets Real Real -> Geo-Standard-Insets]
                                   [Geo-Insets-Datum Real -> Geo-Insets-Datum]
                                   [Geo-Insets-Datum Real Real -> Geo-Insets-Datum])
  (case-lambda
    [(self s) (geo-insets-scale self s s)]
    [(self sx0 sy0)
     (cond [(and (= sx0 1.0) (= sy0 1.0)) self]
           [else (let-values ([(sx sy) (values (abs (real->double-flonum sx0)) (abs (real->double-flonum sy0)))]
                              [(t r b l) (geo-inset-values self)])
                   (geo-standard-insets (if (< sy0 0) (* b sy) (* t sy))
                                        (if (< sx0 0) (* l sx) (* r sx))
                                        (if (< sy0 0) (* t sy) (* b sy))
                                        (if (< sx0 0) (* r sx) (* l sx))))])]))

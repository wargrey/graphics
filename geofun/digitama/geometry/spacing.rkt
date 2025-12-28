#lang typed/racket/base

(provide (all-defined-out))

(require digimon/sequence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Spacing (U Nonnegative-Real (Listof Nonnegative-Real)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-spacing-values : (-> (Option Geo-Spacing)
                                 (Values Nonnegative-Flonum Nonnegative-Flonum
                                         Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [self]
    (cond [(list? self) (list->4:values (map real->double-flonum self) 0.0)]
          [(real? self) (let ([fl (real->double-flonum self)]) (values fl fl fl fl))]
          [else (values 0.0 0.0 0.0 0.0)])))

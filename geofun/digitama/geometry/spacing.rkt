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

(define geo-spacing-scale : (case-> [Geo-Spacing Real -> Geo-Spacing]
                                    [Geo-Spacing Real Real -> Geo-Spacing])
  (case-lambda
    [(self s) (geo-spacing-scale self s s)]
    [(self sx0 sy0)
     (let-values ([(sx sy) (values (abs sx0) (abs sy0))]
                  [(t r b l) (geo-spacing-values self)])
       (list (if (< sy0 0) (* b sy) (* t sy))
             (if (< sx0 0) (* l sx) (* r sx))
             (if (< sy0 0) (* t sy) (* b sy))
             (if (< sx0 0) (* r sx) (* l sx))))]))

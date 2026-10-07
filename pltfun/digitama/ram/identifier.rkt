#lang typed/racket/base

(provide (all-defined-out))

(require racket/keyword)

(require "style.rkt")
(require "variable.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ram-identify : (-> C-Variable-Datum Symbol (Values Symbol (Option (Expr-Slot-Style RAM-Block-Style))))
  (lambda [self segment]
    (cond [(c-variable? self)
           (let ([var (c-variable-name self)])
             (if (keyword? var)
                 (let ([vname (string->symbol (keyword->immutable-string var))])
                   (ram-theme-adjust vname self (default-ram-pointer-theme-adjuster) (default-ram-pointer-style) segment))
                 (ram-theme-adjust var self (default-ram-variable-theme-adjuster) (default-ram-variable-style) segment)))]
          [(c-vector? self)
           (let ([var (c-vector-name self)])
             (if (keyword? var)
                 (let ([vname (string->symbol (keyword->immutable-string var))])
                   (ram-theme-adjust vname self (default-ram-pointer-theme-adjuster) (default-ram-pointer-style) segment))
                 (ram-theme-adjust var self (default-ram-array-theme-adjuster) (default-ram-array-style) segment)))]
          [else (ram-theme-adjust '|| self (default-ram-padding-theme-adjuster) (default-ram-padding-style) segment)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) ram-theme-adjust : (-> Symbol C-Placeholder (Option (RAM-Location-Theme-Adjuster S)) (Expr-Slot-Style S) Symbol
                                            (Values Symbol (Expr-Slot-Style S)))
  (lambda [variable content style-adjust style segment]
    (values variable
            (expr-slot-theme-adjust style variable content style-adjust segment))))

#lang typed/racket/base

(provide (all-defined-out))

(require racket/keyword)

(require "style.rkt")
(require "variable.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ram-identify : (-> C-Variable-Datum Symbol (Values Symbol (Option RAM-Block-Style)))
  (lambda [self segment]
    (cond [(c-variable? self)
           (let ([var (c-variable-name self)])
             (if (keyword? var)
                 (let ([vname (string->symbol (keyword->immutable-string var))])
                   (ram-style-construct vname self (default-ram-pointer-style-make) make-ram-pointer-style segment))
                 (ram-style-construct var self (default-ram-variable-style-make) make-ram-variable-style segment)))]
          [(c-vector? self)
           (let ([var (c-vector-name self)])
             (if (keyword? var)
                 (let ([vname (string->symbol (keyword->immutable-string var))])
                   (ram-style-construct vname self (default-ram-pointer-style-make) make-ram-pointer-style segment))
                 (ram-style-construct var self (default-ram-array-style-make) make-ram-array-style segment)))]
          [else (ram-style-construct '|| self (default-ram-padding-style-make) make-ram-padding-style segment)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) ram-style-construct : (-> Symbol C-Placeholder (Option (RAM-Location-Style-Make (∩ S RAM-Block-Style))) (-> (∩ S RAM-Block-Style)) Symbol
                                               (Values Symbol (∩ S RAM-Block-Style)))
  (lambda [variable content mk-style mk-fallback-style segment]
    (values variable
            ((inst expr-slot-style-construct C-Placeholder (∩ S RAM-Block-Style) Symbol)
             variable content mk-style mk-fallback-style segment))))

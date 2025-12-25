#lang typed/racket/base

(provide (all-defined-out))

(require racket/keyword)

(require "style.rkt")
(require "variable.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ram-identify : (-> C-Variable-Datum Symbol (Values Symbol (Option RAM-Block-Style)))
  (lambda [self segment]
    (cond [(c-variable? self)
           (if (keyword? (c-variable-name self))
               (let ([vname (string->symbol (keyword->immutable-string (c-variable-name self)))])
                 (ram-style-construct vname segment (default-ram-pointer-style-make) make-ram-pointer-style))
               (ram-style-construct (c-variable-name self) segment (default-ram-variable-style-make) make-ram-variable-style))]
          [(c-vector? self)
           (if (keyword? (c-vector-name self))
               (let ([vname (string->symbol (keyword->immutable-string (c-vector-name self)))])
                 (ram-style-construct vname segment (default-ram-pointer-style-make) make-ram-pointer-style))
               (ram-style-construct (c-vector-name self) segment (default-ram-array-style-make) make-ram-array-style))]
          [else (ram-style-construct '|| segment (default-ram-padding-style-make) make-ram-padding-style)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) ram-style-construct : (-> Symbol Symbol (Option (RAM-Location-Style-Make (∩ S RAM-Block-Style)))
                                               (-> (∩ S RAM-Block-Style))
                                               (Values Symbol (∩ S RAM-Block-Style)))
  (lambda [variable segment mk-style mk-fallback-style]
    (values variable
            ((inst dia-block-style-construct Symbol (∩ S RAM-Block-Style) Symbol)
             variable mk-style mk-fallback-style segment))))
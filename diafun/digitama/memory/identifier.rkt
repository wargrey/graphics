#lang typed/racket/base

(provide (all-defined-out))

(require racket/keyword)

(require "style.rkt")
(require "variable.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define memory-identify : (-> (U C-Variable C-Pad) Symbol (Values Symbol (Option Memory-Location-Style)))
  (lambda [self segment]
    (cond [(c-padding? self) (memory-style-construct '|| segment (default-memory-padding-style-make) make-memory-padding-style)]
          [(keyword? (c-variable-name self))
           (let ([vname (string->symbol (keyword->immutable-string (c-variable-name self)))])
             (memory-style-construct vname segment (default-memory-variable-style-make) make-memory-variable-style))]
          [else (memory-style-construct (c-variable-name self) segment (default-memory-temporary-style-make) make-memory-temporary-style)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) memory-style-construct : (-> Symbol Symbol (Option (Memory-Location-Style-Make (∩ S Memory-Location-Style)))
                                                  (-> (∩ S Memory-Location-Style))
                                                  (Values Symbol (∩ S Memory-Location-Style)))
  (lambda [variable segment mk-style mk-fallback-style]
    (values variable
            ((inst dia-node-style-construct (∩ S Memory-Location-Style) Symbol) variable mk-style mk-fallback-style segment))))

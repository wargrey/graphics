#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/digicore.rkt")

(define css-set! : (-> CSS-Values Symbol Any Void)
  (lambda [declared-values property value]
    (hash-set! (css-values-descriptors declared-values) property
               (λ [] value))))

(define css-values-fold : (All (a) (-> CSS-Values a (-> Symbol Any a a) a))
  (lambda [css-values initial fold]
    (for/fold ([val++ : a initial])
              ([(property fvalue) (in-hash (css-values-descriptors css-values))])
      (fold property (fvalue) val++))))

(define css-values-for-each : (All (a) (case-> [CSS-Values a (-> Symbol Any a Any) -> Void]
                                               [CSS-Values (-> Symbol Any Any) -> Void]))
  (case-lambda
    [(css-values env iter)
     (for ([(property fvalue) (in-hash (css-values-descriptors css-values))])
       (iter property (fvalue) env))]
    [(css-values iter)
     (for ([(property fvalue) (in-hash (css-values-descriptors css-values))])
       (iter property (fvalue)))]))
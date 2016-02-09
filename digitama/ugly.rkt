#lang typed/racket/base

(provide make-is-a?)

(require typed/racket/unsafe)

(module ugly racket/base
  (provide (all-defined-out))

  (require racket/class)
  
  (define make-is-a?
    (lambda [c]
      (λ [v] (is-a? v c)))))

(unsafe-require/typed
 (submod "." ugly)
 [make-is-a? (All (%) (-> % (-> Any Boolean : #:+ (Instance %))))])

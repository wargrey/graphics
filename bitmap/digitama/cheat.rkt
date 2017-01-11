#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/class)
(require typed/racket/unsafe)

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))

(module cheat racket/base (provide (all-defined-out))
  (define make-cheat-opaque? (λ [? fname] (procedure-rename (λ [v] (and (? v) #true)) fname))))

(unsafe-require/typed
 (submod "." cheat)
 [make-cheat-opaque? (All (FT) (->* ((U (-> Any Boolean) Byte)) ((Option Symbol)) (-> Any Boolean : #:+ FT)))])

(define-syntax (define-cheat-opaque stx)
  (syntax-case stx []
    [(_ id #:is-a? % arg%)
     #'(define id : (-> Any Boolean : #:+ (Instance %))
         ((inst make-cheat-opaque? (Instance %)) (λ [v] (is-a? v arg%)) 'id))]))

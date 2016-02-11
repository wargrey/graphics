#lang typed/racket

(require typed/racket/unsafe)

(define-syntax (unsafe-require/typed/provide stx)
  (syntax-case stx []
    [(_ modpath [id Type] ...)
     #'(begin (provide id ...)
              (unsafe-require/typed modpath [id Type] ...))]))

(module ugly racket/base
  (provide (all-defined-out))

  (require racket/class)
  
  (define make-is-a?
    (lambda [c]
      (λ [v] (is-a? v c))))

  (define make-subclass?
    (lambda [c]
      (λ [v] (subclass? v c)))))

(unsafe-require/typed/provide
 (submod "." ugly)
 [make-is-a? (All (%) (-> % (-> Any Boolean : #:+ (Instance %))))]
 [make-subclass? (All (%) (-> % (-> Any Boolean : #:+ %)))])

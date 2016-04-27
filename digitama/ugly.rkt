#lang typed/racket

(provide (all-defined-out))

(require typed/racket/unsafe)

(require (for-syntax racket/syntax))

(define-syntax (unsafe-require/typed/provide stx)
  (syntax-case stx []
    [(_ modpath [id Type] ...)
     #'(begin (provide id ...)
              (unsafe-require/typed modpath [id Type] ...))]))

(module ugly racket/base
  (provide (all-defined-out))

  (require racket/class)
  
  (define make-is-a?
    (lambda [c%]
      (λ [v] (is-a? v c%))))

  (define make-subclass?
    (lambda [c%]
      (λ [v] (subclass? v c%))))

  (define make-procedure?
    (lambda [arity [ok? #false]]
      (λ [v] (and (procedure? v)
                  (procedure-arity-includes? v arity ok?))))))

(unsafe-require/typed/provide
 (submod "." ugly)
 [make-is-a? (All (%) (-> % (-> Any Boolean : #:+ (Instance %))))]
 [make-subclass? (All (%) (-> % (-> Any Boolean : #:+ %)))]
 [make-procedure? (All (P) (->* (Byte) (Boolean) (-> Any Boolean : #:+ P)))])

(define-syntax (define/make-is-a? stx)
  (syntax-case stx [:]
    [(_ % : Type% class-definition)
     (with-syntax ([%? (format-id #'% "~a?" (syntax-e #'%))])
       #'(begin (define % : Type% class-definition)
                (define %? (cond [(class? %) ((inst make-is-a? Type%) %)]
                                 [else (raise-result-error 'define/make-is-a? "class?" %)]))))]))

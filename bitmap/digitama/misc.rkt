#lang typed/racket/base

(provide (all-defined-out))

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax racket/sequence))
(require (for-syntax syntax/parse))

(define-syntax (require/provide stx)
  (syntax-case stx []
    [(_ spec ...)
     #'(begin (provide (all-from-out spec)) ...
              (require spec) ...)]))

(define-syntax (struct: stx)
  (syntax-case stx [:]
    [(_ id : ID rest ...)
     (with-syntax ([make-id (format-id #'id "make-~a" (syntax-e #'id))])
       #'(begin (struct id rest ... #:extra-constructor-name make-id #:transparent)
                (define-type ID id)))]))

(define-syntax (define-enumeration stx)
  (syntax-case stx [:]
    [(_ id : TypeU #:with kw->enum #:-> EnumType [enum value] ...)
     #'(begin (define-enumeration id : TypeU enum ...)
              (define kw->enum : (case-> [TypeU -> EnumType] [Symbol -> (Option EnumType)])
                (lambda [kw] (cond [(eq? kw 'enum) value] ... [else #false]))))]
    [(_ [id ids] : TypeU enum ...)
     (with-syntax ([id? (format-id #'id "~a?" (syntax-e #'id))])
       #'(begin (define-type TypeU (U 'enum ...))
                (define ids : (Pairof TypeU (Listof TypeU)) (list 'enum ...))
                (define id? : (-> Any Boolean : #:+ TypeU) (λ [v] (or (eq? v 'enum) ...)))))]
    [(_ id : TypeU enum ...)
     (with-syntax ([ids (format-id #'id "~as" (syntax-e #'id))])
       #'(define-enumeration [id ids] : TypeU enum ...))]))

(define-syntax (define-enumeration* stx)
  (syntax-parse stx
    [(_ id #:as TypeU kw-filter #:-> [args Args] ... Type [(enum) sexp ...] ... [#:else defsexp ...])
     #'(begin (define-enumeration id : TypeU enum ...)
              (define (kw-filter [kw : Symbol] [args : Args] ...) : Type
                (cond [(eq? kw 'enum) sexp ...] ... [else defsexp ...])))]
    [(_ id #:as TypeU kw-filter #:-> [args Args] ... Type [(enum) sexp ...] ...)
     #'(begin (define-enumeration id : TypeU enum ...)
              (define kw-filter : (case-> [TypeU Args ... -> Type] [Symbol Args ... -> (Option Type)])
                (lambda [kw args ...] (cond [(eq? kw 'enum) sexp ...] ... [else #false]))))]
    [(_ id #:+> TypeU kw->enum enum->kw #:-> Type [enum value] ... [enum$ value$])
     (with-syntax ([(range ...) (for/list ([<start> (in-syntax #'(value ...))]
                                           [<end> (sequence-tail (in-syntax #'(value ... value$)) 1)])
                                  (datum->syntax <start> (/ (+ (syntax-e <start>) (syntax-e <end>)) 2)))])
       #'(begin (define-enumeration id : TypeU #:with kw->enum #:-> Type [enum value] ... [enum$ value$])
                (define (enum->kw [kv : Type]) : TypeU
                  (cond [(< kv range) 'enum] ... [else 'enum$]))))]
    [(_ id #:+> TypeU kw->enum enum->kw [start:integer enum ... enum$])
     (with-syntax ([(value ... value$) (for/list ([<enum> (in-syntax #'(enum ... enum$))]
                                                  [idx (in-naturals 0)])
                                         (datum->syntax <enum> (+ (syntax-e #'start) idx)))])
       #'(begin (define-enumeration id : TypeU #:with kw->enum #:-> Integer [enum value] ... [enum$ value$])
                (define (enum->kw [kv : Integer]) : TypeU
                  (cond [(= kv value) 'enum] ... [else 'enum$]))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define require-image : (-> String Symbol Positive-Real Any)
  (lambda [src.rkt id density]
    (define fallback (λ [] (call-with-values (λ [] (eval id (module->namespace src.rkt))) (λ _ (car _)))))
    (module-declared? src.rkt #true)
    (define value (dynamic-require src.rkt id fallback))
    (cond [(not (hash? value)) value]
          [else (hash-ref value (exact->inexact density)
                          (λ [] (let ([all (sort (hash-keys value) >)])
                                  (if (pair? all) (hash-ref value (car all)) value))))])))

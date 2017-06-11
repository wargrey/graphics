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

(define-syntax (enumeration: stx)
  (syntax-case stx [:]
    [(_ [id ids] : TypeU enum ...)
     (with-syntax ([id? (format-id #'id "~a?" (syntax-e #'id))])
       #'(begin (define-type TypeU (U 'enum ...))
                (define ids : (Pairof TypeU (Listof TypeU)) (list 'enum ...))
                (define-predicate id? TypeU)))]
    [(_ id : TypeU enum ...)
     (with-syntax ([ids (format-id #'id "~as" (syntax-e #'id))])
       #'(enumeration: [id ids] : TypeU enum ...))]))

(define-syntax (define-css-keywords stx)
  (syntax-parse stx
    [(_ id #:as TypeU kw-map #:-> [args Args] ... Type [(enum) sexp ...] ... [#:else otherwise ...])
     #'(begin (enumeration: id : TypeU enum ...)
              (define (kw-map [kw : Symbol] [args : Args] ...) : Type (case kw [(enum) sexp ...] ... [else otherwise ...])))]
    [(_ id #:as TypeU kw-map #:-> [args Args] ... Type [(^enum) ^sexp ...] [(enum) sexp ...] ...)
     #'(begin (enumeration: id : TypeU ^enum enum ...)
              (define (kw-map [kw : Symbol] [args : Args] ...) : Type (case kw [(enum) sexp ...] ... [else ^sexp ...])))]
    [(_ id #:+> TypeU kw->enum enum->kw #:-> Type #:fallback fbenum [enum value] ... [enum$ value$])
     (with-syntax ([fbvalue (for/first ([<enum> (in-syntax #'(enum ... enum$))]
                                        [<value> (in-syntax #'(value ... value$))]
                                        #:when (eq? (syntax-e <enum>) (syntax-e #'fbenum)))
                              <value>)]
                   [(range ...) (for/list ([<start> (in-syntax #'(value ...))]
                                           [<end> (sequence-tail (in-syntax #'(value ... value$)) 1)])
                                  (datum->syntax <start> (/ (+ (syntax-e <start>) (syntax-e <end>)) 2)))])
       #'(begin (enumeration: id : TypeU enum ... enum$)
                (define (kw->enum [kw : Symbol]) : Type (case kw [(enum) value] ... [(enum$) value$] [else fbvalue]))
                (define (enum->kw [kv : Type]) : TypeU (cond [(< kv range) 'enum] ... [else 'enum$]))))]
    [(_ id #:+> TypeU kw->enum enum->kw #:fallback fbenum [start:nat enum ... enum$])
     (with-syntax ([value$ (datum->syntax #'enum$ (length (syntax->list #'(enum ... enum$))))]
                   [fbvalue (for/first ([<enum> (in-syntax #'(enum ...))]
                                        [idx (in-naturals (syntax-e #'start))]
                                        #:when (eq? (syntax-e <enum>) (syntax-e #'fbenum)))
                              (datum->syntax <enum> idx))]
                   [(value ...) (for/list ([<enum> (in-syntax #'(enum ...))]
                                           [idx (in-naturals (syntax-e #'start))])
                                  (datum->syntax <enum> idx))])
       #'(begin (enumeration: id : TypeU enum ... enum$)
                (define (kw->enum [kw : Symbol]) : Integer (case kw [(enum) value] ... [(enum$) value$] [else fbvalue]))
                (define (enum->kw [kv : Integer]) : TypeU (case kv [(value) 'enum] ... [else 'enum$]))))]))

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

#lang typed/racket/base

(provide (all-defined-out))

(require racket/promise)

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))

(define-syntax (struct: stx)
  (syntax-case stx [:]
    [(_ id : ID rest ...)
     (with-syntax ([make-id (format-id #'id "make-~a" (syntax-e #'id))])
       #'(begin (struct id rest ... #:extra-constructor-name make-id #:transparent)
                (define-type ID id)))]))

(define-syntax (define-css-parameter stx)
  (syntax-case stx [:]
    [(_ id : Type #:= defval)
     #'(define id : (case-> [(U Type (Promise (-> Type))) -> Void] [-> Type])
         (let ([&storage : (Boxof (-> Type)) (box (λ [] defval))])
           (case-lambda
             [(v) (set-box! &storage (if (promise? v) (force v) (λ [] v)))]
             [() ((unbox &storage))])))]
    [(_ id : Type #:guard [guard : TypeIn] #:= defval)
     #'(define id : (case-> [TypeIn -> Void] [-> Type])
         (let ([&storage : (Boxof Type) (box (guard defval))])
           (case-lambda
             [(v) (set-box! &storage (guard v))]
             [() (unbox &storage)])))]))

(define-syntax (define-css-parameters stx)
  (syntax-case stx [:]
    [(_ parameters [name ...] : Type #:= defval)
     (with-syntax ([(p-name ...) (for/list ([<u> (in-list (syntax->list #'(name ...)))]) (format-id <u> "css-~a" (syntax-e <u>)))])
       #'(begin (define-css-parameter p-name : Type #:= defval) ... 
                (define parameters : (case-> [-> (Listof (Pairof Symbol (U Type (Promise (-> Type)))))]
                                             [(Listof (Pairof Symbol Type)) -> Void])
                  (case-lambda
                    [(db) (let ([pv (assq 'name db)]) (when pv (p-name (cdr pv)))) ...]
                    [() (list (cons 'name (p-name)) ...)]))))]))

(define-type (Listof+ css) (Pairof css (Listof css)))
(define-type Symbol↯ Symbol)
(define-type Keyword↯ Keyword)

(define css-log-error : (->* ((U exn String)) (Any Log-Level Symbol) Void)
  (lambda [errobj [src #false] [level 'debug] [topic 'exn:css:fail]]
    (define message : String (if (string? errobj) errobj (format "@~s: ~a: ~a" src (object-name errobj) (exn-message errobj))))
    (log-message (current-logger) level topic message errobj)))
  
(define css-log-read-error : (->* ((U exn String)) (Any Log-Level) Void)
  (lambda [errobj [src #false] [level 'debug]]
    (css-log-error errobj src level 'exn:css:read)))

(define css-log-eval-error : (->* ((U exn String)) (Any Log-Level) Void)
  (lambda [errobj [src #false] [level 'debug]]
    (css-log-error errobj src level 'exn:css:eval)))

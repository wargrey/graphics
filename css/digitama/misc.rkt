#lang typed/racket/base

(provide (all-defined-out))

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))

(define-syntax (struct: stx)
  (syntax-case stx [:]
    [(_ id : ID rest ...)
     (with-syntax ([make-id (format-id #'id "make-~a" (syntax-e #'id))])
       #'(begin (struct id rest ... #:extra-constructor-name make-id #:transparent)
                (define-type ID id)))]))

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

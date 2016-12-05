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

#lang typed/racket/base

(provide (all-defined-out))

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (require/provide stx)
  (syntax-case stx []
    [(_ spec ...)
     (syntax/loc stx
       (begin (provide (all-from-out spec)) ...
              (require spec) ...))]))

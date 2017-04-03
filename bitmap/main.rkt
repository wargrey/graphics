#lang typed/racket/base

(provide (all-defined-out))

(require (for-syntax racket/base))

(define-syntax (require/provide stx)
  (syntax-case stx []
    [(_ spec ...)
     #'(begin (provide (all-from-out spec)) ...
              (require spec) ...)]))

(require/provide colorspace)
(require/provide typed/images/icons typed/images/logos)

(require/provide "digitama/digicore.rkt")
(require/provide "base.rkt" "constructor.rkt" "combiner.rkt")
(require/provide "resize.rkt" "misc.rkt" "compile-time.rkt")
(require/provide "color.rkt" "font.rkt")

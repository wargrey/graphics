#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require (for-syntax racket/base))

(define-syntax (unsafe/require/provide stx)
  (syntax-case stx [:]
    [(_ modpath [id Type] ...)
     #'(begin (provide id ...)
              (unsafe-require/typed modpath [id Type] ...))]
    [(_ modpath [#:opaque [Predicate predicate] ...] [id Type] ...)
     #'(begin (provide Predicate predicate) ...
              (unsafe-require/typed modpath [#:opaque Predicate predicate] ...)
              (unsafe/require/provide modpath [id Type] ...))]))

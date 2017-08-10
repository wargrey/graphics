#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require (for-syntax racket/base))

(define-syntax (unsafe/require/provide stx)
  (syntax-case stx [:]
    [(_ modpath [#:opaque [PType pid?] ...] [id Type] ...)
     #'(begin (provide PType pid?) ...
              (unsafe-require/typed modpath [#:opaque PType pid?]) ...
              (unsafe/require/provide modpath [id Type] ...))]
    [(_ modpath [id Type] ...)
     #'(begin (provide id ...)
              (unsafe-require/typed modpath [id Type] ...))]))

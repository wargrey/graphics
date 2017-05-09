#lang typed/racket/base

(provide (all-defined-out))
(provide Brush-Pattern)

(require typed/racket/unsafe)

(module unsafe racket/base
  (provide (all-defined-out))
  (provide (rename-out [cpointer? brush-pattern?]))
  
  (require "ffi.rkt"))

(define-type Brush-Pattern* (U False Brush-Pattern FlVector))

(unsafe-require/typed
 (submod "." unsafe)
 [#:opaque Brush-Pattern brush-pattern?])

#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)
(require "unsafe/surface/type.rkt")

(module unsafe racket/base
  (provide (all-defined-out))

  (require "unsafe/surface/abstract.rkt")

  (define create-abstract-surface
    (lambda [flwidth flheight density scale?]
      (define-values (surface cr width height) (cairo-create-abstract-surface* flwidth flheight density scale?))
      (values surface cr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [create-abstract-surface (All (S) (Cairo-Surface-Create S))])

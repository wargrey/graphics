#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "surface/type.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "surface/bitmap.rkt")
 [create-argb-bitmap (All (S) (Cairo-Surface-Create* S))])

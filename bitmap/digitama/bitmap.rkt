#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)
(require pangocairo/digitama/unsafe/surface/type)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "unsafe/surface.rkt")
 [create-argb-bitmap (All (S) (Cairo-Surface-Create* S))])

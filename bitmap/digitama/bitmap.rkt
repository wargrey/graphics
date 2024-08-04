#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)
(require geofun/digitama/unsafe/surface/type)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 "unsafe/surface.rkt"
 [create-argb-bitmap (All (S) (Cairo-Surface-Create S))])

#lang typed/racket/base

(provide (all-defined-out) 3D-Radius-Type)
(provide geo-icosahedron-side-projection geo:icosahedron:side? Geo:Icosahedron:Side)
(provide geo-icosahedron-over-projection geo:icosahedron:over? Geo:Icosahedron:Over)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require "digitama/dc/projection.rkt")
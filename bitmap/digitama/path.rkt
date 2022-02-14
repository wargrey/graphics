#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct path<%>
  ([footprints : (Listof Float-Complex)]
   [commands : (Listof Char)]
   [anchors : (HashTable Symbol Float-Complex)])
  #:type-name Path<%>
  #:transparent)

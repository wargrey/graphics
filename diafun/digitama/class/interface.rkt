#lang typed/racket/base

(provide (all-defined-out))

(require racket/case)

(require "node.rkt")
(require "../path/interface.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diacls-node-fallback-construct : Dia-Path-Id->Node-Shape
  (lambda [id label style width height direction hint]
    (case/eq (object-name style)
             [(diacls-interface-style) (diacls-block-interface id label style width height direction hint)]
             [(diacls-class-style)     (diacls-block-class id label style width height direction hint)])))

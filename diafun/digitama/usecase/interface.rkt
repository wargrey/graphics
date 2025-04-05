#lang typed/racket/base

(provide (all-defined-out))

(require racket/case)

(require "style.rkt")
(require "node.rkt")
(require "../path/interface.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diauc-node-fallback-construct : Dia-Path-Id->Node-Shape
  (lambda [id label style width height direction hint]
    (case/eq (object-name style)
             [(diauc-actor-style) (diauc-block-actor id label style width height direction hint)]
             [(diauc-ucase-style) (diauc-block-ucase id label style width height direction hint)])))

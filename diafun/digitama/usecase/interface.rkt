#lang typed/racket/base

(provide (all-defined-out))

(require racket/case)

(require "block.rkt")
(require "../track/interface.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diauc-block-fallback-construct : Dia-Anchor->Block
  (lambda [id label style width height direction hint]
    (case/eq (object-name style)
             [(diauc-actor-style) (diauc-block-actor id label style width height direction hint)]
             [(diauc-ucase-style) (diauc-block-ucase id label style width height direction hint)])))

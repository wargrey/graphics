#lang typed/racket/base

(provide (all-defined-out))

(require "../block/interface.rkt")
(require "../block/dc/node.rkt")

(require "parameter.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) cls-block-interface : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction stereotype]
    (dia-block-rectangle/cr:8th block-key caption style width height direction stereotype
                                (or stereotype 'Interface) (default-cls-stereotype-font))))

(define #:forall (S) cls-block-class : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction stereotype]
    (dia-block-rectangle/cr:8th block-key caption style width height direction stereotype
                                stereotype (default-cls-stereotype-font))))

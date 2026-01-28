#lang typed/racket/base

(provide (all-defined-out))

(require "../block/interface.rkt")
(require "../block/dc/node.rkt")

(require "parameter.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) cls-block-interface : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction stereotype]
    (dia-block-rectangle/cr:8th block-key
                                (or (dia-caption+stereotype caption (or stereotype 'Interface) style
                                                            (default-cls-stereotype-font) (default-cls-stereotype-gapsize)
                                                            height)
                                    caption)
                                style width height direction stereotype)))

(define #:forall (S) cls-block-class : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction stereotype]
    (dia-block-rectangle/cr:8th block-key
                                (or (dia-caption+stereotype caption stereotype style
                                                            (default-cls-stereotype-font) (default-cls-stereotype-gapsize)
                                                            height)
                                    caption)
                                style width height direction stereotype)))

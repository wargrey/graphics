#lang typed/racket/base

(provide (all-defined-out))

(require "self.rkt")

(require "../dc/composite.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:trail geo:group
  ([self : Geo:Track])
  #:type-name Geo:Trail
  #:transparent)

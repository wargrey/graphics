#lang typed/racket/base

(provide (all-defined-out))

(require "../track/dc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct dia:use-case dia:track ()
  #:type-name Dia:Use-Case
  #:transparent)

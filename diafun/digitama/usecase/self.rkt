#lang typed/racket/base

(provide (all-defined-out))

(require "../track/self.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct dia:use-case dia:track ()
  #:type-name Dia:Use-Case
  #:transparent)

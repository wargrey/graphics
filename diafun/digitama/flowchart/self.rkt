#lang typed/racket/base

(provide (all-defined-out))

(require "../track/self.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct dia:flow dia:track ()
  #:type-name Dia:Flow
  #:transparent)

#lang typed/racket/base

(provide (all-defined-out))

(require "../path/self.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct dia:flow dia:path ()
  #:type-name Dia:Flow
  #:transparent)

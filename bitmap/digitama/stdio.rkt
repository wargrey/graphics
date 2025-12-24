#lang typed/racket/base

(provide (all-defined-out))

(require "self.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-bitmap : (->* ((U Path-String Input-Port)) (#:backing-scale Positive-Flonum #:try-@2x? Boolean) Bitmap)
  (lambda [/dev/stdin #:backing-scale [density 1.0] #:try-@2x? [try-@2x? #false]]
    (error 'read-bitmap "TODO")))

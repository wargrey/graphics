#lang racket/base

(provide (all-defined-out))

(require ffi/unsafe)
(require ffi/unsafe/objc)

(require racket/class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-platform-surface nsview width height)
  (cpointer-tag (objc-get-class nsview)))

#lang typed/racket/base

(provide (all-defined-out))

(require "../visual/ctype.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Cairo-Surface-Create S) (-> Flonum Flonum Flonum Boolean (Values S Cairo-DC)))

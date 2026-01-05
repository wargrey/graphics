#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-rich-unit-needs-gap-space? : (-> String Boolean)
  (lambda [v]
    #true))

(define geo-rich-unit-can-omit-one? : (-> String Boolean)
  (lambda [v]
    #false))

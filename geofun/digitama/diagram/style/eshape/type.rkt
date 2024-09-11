#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Option-Edge-Shape (Option Geo-Edge-Shape))
(define-type Maybe-Edge-Shape (U Void Option-Edge-Shape))

(struct Geo-Edge-Shape ())

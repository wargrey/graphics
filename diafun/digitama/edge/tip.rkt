#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Option-Edge-Tip-Shape (Option Dia-Edge-Tip-Shape))
(define-type Maybe-Edge-Tip-Shape (U Void Option-Edge-Tip-Shape))

(struct Dia-Edge-Tip-Shape ())

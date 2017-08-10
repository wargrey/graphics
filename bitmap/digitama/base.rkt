#lang typed/racket/base

(provide (all-defined-out) nan? infinite? exact-round)
(provide (all-from-out racket/flonum racket/fixnum))

(require racket/flonum)
(require racket/fixnum)
(require (only-in racket/math nan? infinite? exact-round))

(define-type FlRGBA rgba)
(define-type Color (U Symbol Integer FlColor))

(struct Paint () #:transparent)
(struct FlColor () #:transparent)
(struct rgba FlColor ([red : Flonum] [green : Flonum] [blue : Flonum] [alpha : Flonum]) #:transparent)

(define default-bitmap-density : (Parameterof Positive-Flonum) (make-parameter 2.0))
(define default-bitmap-icon-height : (Parameterof Nonnegative-Flonum) (make-parameter 24.0))

#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out racket/flonum racket/fixnum))

(provide make-object send get-face-list)
(provide nan? infinite? exact-round)
(provide bitmap% Bitmap% make-bitmap read-bitmap)

(require racket/flonum)
(require racket/fixnum)

(require (only-in racket/class make-object send))
(require (only-in racket/math nan? infinite? exact-round))
(require (only-in typed/racket/draw bitmap% Bitmap% make-bitmap read-bitmap))
(require (only-in typed/racket/draw get-face-list))

(define-type FlRGBA rgba)
(define-type Bitmap (Instance Bitmap%))
(define-type Color (U Symbol Integer FlColor))

(struct Paint () #:transparent)
(struct FlColor () #:transparent)
(struct rgba FlColor ([red : Flonum] [green : Flonum] [blue : Flonum] [alpha : Flonum]) #:transparent)

(define default-bitmap-density : (Parameterof Positive-Flonum) (make-parameter 2.0))
(define default-bitmap-icon-height : (Parameterof Nonnegative-Flonum) (make-parameter 24.0))

(define the-invalid-image : Bitmap (read-bitmap (open-input-bytes #"placeholder")))

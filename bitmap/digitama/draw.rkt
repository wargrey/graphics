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

(define-type Bitmap (Instance Bitmap%))
(define-type Color (U Symbol Integer FlColor))

(define-type Paint paint)
(define-type FlColor flcolor)
(define-type FlRGBA rgba)

(struct paint () #:transparent)
(struct flcolor () #:transparent)
(struct rgba flcolor ([red : Flonum] [green : Flonum] [blue : Flonum] [alpha : Flonum]) #:transparent)


#lang typed/racket/base

(provide (all-defined-out))

(provide make-object send)
(provide bitmap% Bitmap% make-bitmap read-bitmap)
(provide get-face-list)

(require (only-in racket/class make-object send))
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


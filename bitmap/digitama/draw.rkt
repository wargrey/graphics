#lang typed/racket/base

(provide (all-defined-out))

(provide make-object send)
(provide bitmap% Bitmap% make-bitmap read-bitmap)
(provide get-face-list)

(require (only-in racket/class make-object send))
(require (only-in typed/racket/draw bitmap% Bitmap% make-bitmap read-bitmap))
(require (only-in typed/racket/draw get-face-list))

(define-type Bitmap (Instance Bitmap%))

#lang typed/racket/base

(provide bitmap% Bitmap% make-bitmap read-bitmap get-face-list)

(require (only-in typed/racket/draw
                  bitmap% Bitmap% make-bitmap read-bitmap
                  get-face-list))

(module untyped racket/base
  (provide make-object send)
  (provide bitmap% make-bitmap read-bitmap)
  
  (require (only-in racket/class make-object send))
  (require (only-in racket/draw bitmap% make-bitmap read-bitmap)))

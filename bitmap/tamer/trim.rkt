#lang typed/racket/base

(require "../digitama/digicore.rkt")
(require "../constructor.rkt")
(require "../resize.rkt")

(define text : Bitmap (bitmap-text (number->string (current-memory-use))))
(bitmap-frame text)
(bitmap-frame (time (bitmap-trim text)))

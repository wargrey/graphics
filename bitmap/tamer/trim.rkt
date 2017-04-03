#lang typed/racket/base

(require "../constructor.rkt")
(require "../resize.rkt")

(define text (bitmap-text (string-append "Sphinx: " (number->string (current-memory-use)))))
(bitmap-frame text)
(bitmap-frame (time (bitmap-trim text)))

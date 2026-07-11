#lang typed/racket/base

(provide (all-defined-out))

(require "self.rkt")
(require "../stdio.rkt")

(require geofun/digitama/base)

(require psd/bitmap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-read-bitmap bitmap #:-> Bitmap
  (lambda [/dev/stdin density]
    (define signature (peek-nbytes /dev/stdin 4))
    (cond [(equal? signature #"8BPS") (read-psd-bitmap /dev/stdin #:density density)]
          [else (throw-unsupported-error /dev/stdin 'read-bitmap "unrecognized image format")])))

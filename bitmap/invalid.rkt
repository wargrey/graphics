#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/base.rkt")
(require "digitama/unsafe/convert.rkt")
(require "digitama/unsafe/invalid.rkt")

(define the-invalid-bitmap : Bitmap (bitmap_invalid (default-bitmap-density)))

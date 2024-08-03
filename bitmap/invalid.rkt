#lang typed/racket/base

(provide (all-defined-out) bitmap-invalid?)

(require "digitama/base.rkt")
(require "digitama/convert.rkt")
(require "digitama/unsafe/invalid.rkt")

(define the-invalid-bitmap : Bitmap (bitmap_invalid 1.0 1.0 (default-bitmap-density)))

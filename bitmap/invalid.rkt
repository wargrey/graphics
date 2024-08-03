#lang typed/racket/base

(provide (all-defined-out) bitmap-invalid?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require "digitama/convert.rkt")
(require "digitama/unsafe/invalid.rkt")

(require pangocairo/digitama/base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-invalid-bitmap : Bitmap (bitmap_invalid 1.0 1.0 (default-bitmap-density)))

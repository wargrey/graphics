#lang typed/racket/base

(provide (all-defined-out))

(require "../invalid.rkt")
(require "../digitama/unsafe/convert.rkt")

(bitmap-invalid? the-invalid-bitmap)

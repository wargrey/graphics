#lang typed/racket/base

(provide (all-defined-out) bitmap%?)
(provide (all-from-out "draw.rkt"))

(require "draw.rkt")
(require "unsafe/draw.rkt")

(define default-bitmap-density : (Parameterof Positive-Flonum) (make-parameter 2.0))
(define default-bitmap-icon-height : (Parameterof Nonnegative-Flonum) (make-parameter 24.0))

;(define the-dc : (Instance Bitmap-DC%) (make-object bitmap-dc% (make-object bitmap% 1 1)))
(define the-invalid-image : Bitmap (read-bitmap (open-input-bytes #"placeholder")))

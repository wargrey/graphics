#lang typed/racket

(provide (all-defined-out))
(provide (all-from-out "draw.rkt"))
(provide (all-from-out racket/fixnum racket/flonum racket/math))

(require racket/fixnum)
(require racket/flonum)
(require racket/math)

(require "draw.rkt")

(define default-bitmap-density : (Parameterof Positive-Flonum) (make-parameter 2.0))
(define default-bitmap-icon-height : (Parameterof Nonnegative-Flonum) (make-parameter 24.0))

;(define the-dc : (Instance Bitmap-DC%) (make-object bitmap-dc% (make-object bitmap% 1 1)))
(define the-invalid-image : (Instance Bitmap%) (read-bitmap (open-input-bytes #"placeholder")))

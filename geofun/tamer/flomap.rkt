#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (build-sine [x : Nonnegative-Fixnum] [y : Nonnegative-Fixnum] [w : Nonnegative-Fixnum] [h : Nonnegative-Fixnum])
  (define grayscale : Flonum
    (real->double-flonum (* 1/2 (+ 1 (sin (sqrt (+ (sqr (- x (/ w 2.0)))
                                                   (sqr (- y (/ h 2.0))))))))))
  (values 1.0 grayscale grayscale grayscale))


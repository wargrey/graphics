#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (build-sine [x : Nonnegative-Fixnum] [y : Nonnegative-Fixnum] [w : Nonnegative-Fixnum] [h : Nonnegative-Fixnum])
  (define grayscale : Flonum
    (real->double-flonum (* 1/2 (+ 1 (sin (sqrt (+ (sqr (- x (/ w 2.0)))
                                                   (sqr (- y (/ h 2.0))))))))))
  (values 1.0 grayscale grayscale grayscale))

(define (xy->argb [x : Nonnegative-Fixnum] [y : Nonnegative-Fixnum] [w : Nonnegative-Fixnum] [h : Nonnegative-Fixnum])
  (define w+h (+ w h))
  (define w-x (- w x))
  (define h-y (- h y))

  (values (real->double-flonum (/ (+ w-x h-y) w+h))
          (real->double-flonum (/ (+ w-x y)   w+h))
          (real->double-flonum (/ (+ x y)     w+h))
          (real->double-flonum (/ (+ x h-y)   w+h))))

#lang typed/racket/base

(require "../digitama/digicore.rkt")
(require "../constructor.rkt")

(define (build-flomap* [x : Nonnegative-Fixnum] [y : Nonnegative-Fixnum] [w : Nonnegative-Fixnum] [h : Nonnegative-Fixnum])
  (define w+h (fx+ w h))
  (values (/ (fx+ x y) w+h)
          (/ (fx+ (fx- w x) y) w+h)
          (/ (fx+ (fx- w x) (fx- h y)) w+h)
          (/ (fx+ x (fx- h y)) w+h)))

(time (bitmap-rectangular 100 100 build-flomap* #:density 1.00))
(time (bitmap-rectangular 100 100 build-flomap* #:density 1.75))
(time (bitmap-rectangular 100 100 build-flomap* #:density 2.00))

(define (build-flomap [x : Nonnegative-Fixnum] [y : Nonnegative-Fixnum] [w : Nonnegative-Fixnum] [h : Nonnegative-Fixnum])
  (define c (* 1/2 (+ 1 (sin (magnitude (make-rectangular (- x (/ w 2.0)) (- y (/ h 2.0))))))))
  (values 1.0 c c c))

(time (bitmap-rectangular 100 100 build-flomap))

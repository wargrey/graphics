#lang typed/racket/base

(require "../digitama/digicore.rkt")
(require "../resize.rkt")
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

(define concentric-circles (time (bitmap-rectangular 100 100 build-flomap)))
(bitmap-copy concentric-circles)
(bitmap-inset concentric-circles 16.0 16.0 -16.0 -16.0)
(bitmap-scale concentric-circles 2.0 1.0)
(bitmap-rt-crop concentric-circles 64 64)

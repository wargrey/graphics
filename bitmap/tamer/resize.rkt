#lang typed/racket

(require "../draw.rkt")
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
(define plane (time (bitmap-rectangular 100 100 build-flomap* #:density 2.00)))

(bitmap-copy plane)
(bitmap-inset plane 16.0 16.0 -16.0 -16.0)
(bitmap-scale plane 2.0 1.0)
(bitmap-rb-crop plane 64 64)

(define text (bitmap-text (string-append "memory: " (number->string (current-memory-use)))))
(define trimed-text (time (bitmap-trim text)))
(bitmap-frame text)
(bitmap-enclosing-box text)
(bitmap-frame trimed-text)
(bitmap-enclosing-box trimed-text)

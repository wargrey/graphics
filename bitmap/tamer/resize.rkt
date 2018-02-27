#lang typed/racket

(require "../digitama/base.rkt")
(require "../resize.rkt")
(require "../constructor.rkt")

(define (build-flomap* [x : Index] [y : Index] [w : Index] [h : Index])
  (define w+h (+ w h))
  (define w-x (- w x))
  (define h-y (- h y))
  (values (/ (+ x y)     w+h)
          (/ (+ w-x y)   w+h)
          (/ (+ w-x h-y) w+h)
          (/ (+ x h-y)   w+h)))

(time (bitmap-rectangular 100 100 build-flomap* #:density 1.00))
(time (bitmap-rectangular 100 100 build-flomap* #:density 1.75))

(define plane (time (bitmap-rectangular 100 100 build-flomap* #:density 2.00)))
(bitmap-copy plane)
(bitmap-inset plane 16.0 16.0 -16.0 -16.0)
(bitmap-scale plane 2.0 1.0)
(bitmap-rb-crop plane 64 64)

(define text (bitmap-text (string-append "memory: " (number->string (current-memory-use)))))
(define trimed-text (time (bitmap-trim text #false)))
(bitmap-frame text)
(bitmap-enclosing-box text)
(bitmap-frame trimed-text)
(bitmap-enclosing-box trimed-text)

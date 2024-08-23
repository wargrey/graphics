#lang typed/racket/base

(require bitmap)

(require racket/math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-values (diameter dash-width alpha) (values 192 4 1/3))
(define yellow-stroke (desc-stroke #:width dash-width #:opacity 1/2 #:cap 'round #:dash 'short-dash))
(default-stroke-paint (desc-stroke #:width dash-width #:opacity alpha #:cap 'round #:dash 'long-dash))

(define (build-flomap [x : Nonnegative-Fixnum] [y : Nonnegative-Fixnum] [w : Nonnegative-Fixnum] [h : Nonnegative-Fixnum])
  (define grayscale : Flonum
    (real->double-flonum (* 1/2 (+ 1 (sin (sqrt (+ (sqr (- x (/ w 2.0)))
                                                   (sqr (- y (/ h 2.0))))))))))
  (values 1.0 grayscale grayscale grayscale))

(define red-circle (bitmap-ellipse diameter #:fill (rgb* 'red alpha)))
(define green-circle (bitmap-ellipse diameter #:fill (rgb* 'green alpha)))
(define blue-circle (bitmap-ellipse diameter #:fill (rgb* 'blue alpha)))
(define yellow-circle (bitmap-ellipse 124 #:stroke yellow-stroke #:fill (rgb* 'yellow 1/2)))
(define 3pc (bitmap-pin* 1/8 11/48 0 0 (bitmap-pin* 1/3 0 0 0 red-circle green-circle) blue-circle))
(define sine (time (bitmap-rectangular 100 100 build-flomap)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  3pc
  (bitmap-pin 3pc 0 0 yellow-circle 64 64)
  (bitmap-frame (bitmap-pin* 1/8 1/8 0 0 yellow-circle yellow-circle yellow-circle))
  (bitmap-frame (bitmap-pin* -1/8 -1/8 0 0 yellow-circle yellow-circle yellow-circle))
  (bitmap-cc-superimpose* (list 3pc yellow-circle))
  
  (bitmap-pin* 1/5 1/5 0 0 sine sine)
  (bitmap-pin* 1/2 0 0 0 sine sine)
  (bitmap-composite #:operator 'screen sine 50 0 sine)
  (bitmap-composite yellow-circle -50 0 sine))

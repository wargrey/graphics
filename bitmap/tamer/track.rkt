#lang typed/racket/base

(require bitmap/track)

(define drywani (make-dryland-wani 64))

(with-dryland-wani! drywani
  (step-left)
  (step-right)
  (step-down)
  (step-left)
  (step-up))

(bitmap-track* drywani)
(bitmap-track drywani #:color 'blue #:fill 'green)

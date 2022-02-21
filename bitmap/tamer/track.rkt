#lang typed/racket/base

(require bitmap/track)

(define drywani (make-dryland-wani 64))

(with-dryland-wani! drywani
  (step-left)
  (step-right)
  (step-down 2)
  (step-left)
  (step-up)

  (jump-up 2 '#:2nd-home)
  (step-left)
  (step-down)
  (step-left 1 'most-left)
  (close)

  (jump-right-down 2 1 '#:3rd-home)
  (step-up-right)
  (step-right-down)
  (step-down-left)
  (step-left-up)

  (jump-down 3)
  (turn-up-right)
  (turn-right-up)
  (turn-right-down)
  (turn-down-right)
  (turn-down-left)
  (turn-left-down)
  (turn-left-up)
  (turn-up-left)

  (jump-left 3)
  (turn-up-right-down)
  (turn-right-down-left)
  (turn-down-left-up)
  (turn-left-up-right)

  (jump-right 8)
  (turn-up-left-down)
  (turn-left-down-right)
  (turn-down-right-up)
  (turn-right-up-left)

  (jump-back)
  (step-down)

  (jump-back)
  (step-up-right)

  (jump-back)
  (step-left-down 2)

  (step-to 'most-left))

(bitmap-track* drywani)
(bitmap-track drywani #:color 'blue #:fill 'green)

(track-anchor-position drywani '#:home)
(track-anchor-position drywani '#:home #:translate? #true)

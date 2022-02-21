#lang typed/racket/base

(require bitmap/track)
(require bitmap/digitama/track)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-dryland-wani! drywani [64 32] #:-
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
  
  (jump-down-right 3)
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

(track-footprints drywani)
drywani

(track-anchor-position drywani '#:home)
(track-anchor-position drywani '#:home #:translate? #true)

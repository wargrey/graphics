#lang typed/racket/base

(require geofun/track)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-dryland-wani! drywani [64 64] #:-
  (step-left)
  (step-right)
  (step-down 2 '#:rr-angle)
  (step-left 1 '#:lr-angle)
  (step-up 1 'angle-end)
  (jump-back)
  (step-left)
  (jump-back)
  (step-down)

  (jump-back 'angle-end)
  (jump-up 2 '#:z)
  (step-left)
  (step-down)
  (step-left 1 'leftmost)
  (close)
  
  (jump-right-down 2 1 '#:diamond)
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
  
  (jump-down-right 2)
  (turn-up-left-down)
  (turn-left-down-right)
  (turn-down-right-up)
  (turn-right-up-left)
  
  (jump-back)
  (drift '#:home '(-0.5+i -0.5-i))
  
  (jump-back)
  (drift '#:home '())
  
  (jump-back)
  (drift 'leftmost '(-2+i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (require geofun/digitama/track)
  (require geofun/digitama/freeze)
  (require bitmap)

  (default-stroke (desc-stroke #:color 'crimson #:width 2.0))
  
  (reverse (track-footprints drywani))
  drywani
  (geo-freeze drywani #:color 'ForestGreen)

  (let ([bmp (bitmap-square 256)])
    (geo-freeze! bmp drywani -32 -32 #:color 'RoyalBlue)
    bmp)
  
  (track-anchor-position drywani '#:home)
  (track-anchor-position drywani '#:home #:translate? #true))

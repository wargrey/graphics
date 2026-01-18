#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require digimon/measure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct geo-standing-stickman : Geo-Standing-Stickman
  ([head-radius : Length+% 3.5]
   [torso-width : Length+% 4.5]
   [torso-length : Length+% 6.0]
   [leg-width : Length+% 4.0]
   [arm-width : Length+% 3.5]
   [neck-length : Length+% 6.0]
   [upper-arm-length : Length+% 5]
   [lower-arm-length : Length+% 5.5]
   [leg-length : Length+% 13.0]
   [shoulder-breadth : Length+% 7.0]
   [arm-angle : Real (* 2/9 pi)]
   [elbow-angle : Real pi/2]
   [leg-angle : Real pi/4])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-standing-stickman : Geo-Standing-Stickman (make-geo-standing-stickman))

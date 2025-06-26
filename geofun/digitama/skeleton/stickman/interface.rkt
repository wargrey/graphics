#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require digimon/metrics)
(require digimon/constant)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct geo-standing-stickman : Geo-Standing-Stickman
  ([head-radius : Real+% 3.5]
   [torso-width : Real+% 4.5]
   [torso-length : Real+% 6.0]
   [leg-width : Real+% 4.0]
   [arm-width : Real+% 3.5]
   [neck-length : Real+% 6.0]
   [upper-arm-length : Real+% 5]
   [lower-arm-length : Real+% 5.5]
   [leg-length : Real+% 13.0]
   [shoulder-breadth : Real+% 7.0]
   [arm-angle : Real (* 2/9 pi)]
   [elbow-angle : Real pi/2]
   [leg-angle : Real pi/4])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-standing-stickman : Geo-Standing-Stickman (make-geo-standing-stickman))
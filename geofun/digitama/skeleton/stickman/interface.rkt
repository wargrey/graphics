#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct geo-standing-stickman : Geo-Standing-Stickman
  ([head-radius : Real 3.5]
   [torso-width : Real 4.5]
   [leg-width : Real 4.0]
   [arm-width : Real 3.5]
   [neck-length : Real 6.0]
   [torso-length : Real 6.0]
   [upper-arm-length : Real 5]
   [lower-arm-length : Real 5.5]
   [leg-length : Real 13.0]
   [shoulder-breadth : Real 7.0]
   [arm-angle : Real 40.0]
   [elbow-angle : Real 90.0]
   [leg-angle : Real 45.0]
   [radian? : Boolean #false])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-standing-stickman : Geo-Standing-Stickman (make-geo-standing-stickman))
#lang typed/racket

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(for/list : (Listof Geo) ([angle (in-range 0.0 361.0 10.0)])
  (geo-frame
   (geo-cc-superimpose
    (geo-parallelogram 100 50 (degrees->radians angle) #:stroke angle)
    (geo-text angle #:color angle))))

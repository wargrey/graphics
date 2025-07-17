#lang typed/racket/base

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(geo-polybezier* (list 0 100 100+100.0i +100.0i) #:stroke 'RoyalBlue #:fill 'Azure #:close? #true)
(geo-polybezier* (list 0 100 +nan.0 100+100.0i +100.0i) #:stroke 'RoyalBlue #:fill 'Azure)

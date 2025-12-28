#lang typed/racket/base

(require geofun/vector)

(require "../flomap.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-values (width height aradius) (values 100.0 42.0 8))
(define sine (geo-rectangular 128 128 xy->argb))

(define storage (geo-storage #:stroke 'RoyalBlue #:fill 'Azure width height aradius))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (geo-fit-composite #:margin 2.0
                     #:hfit% (abs (- 1.0 (/ (* aradius 2.0) width))) #:vfit% 1.0
                     storage (max (- 0.5 (* (/ aradius width) 0.5)) 0.0) 0.5 sine 0.0 0.5))

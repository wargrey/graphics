#lang typed/racket/base

(require geofun/vector)

(require "../flomap.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-values (diameter dash-width alpha) (values 192 4 1/3))
(define yellow-stroke (desc-stroke #:width dash-width #:opacity 1/2 #:cap 'round #:dash 'short-dash))
(default-border-paint (desc-stroke #:width dash-width #:opacity alpha #:cap 'round #:dash 'long-dash))

(define red-circle (geo-ellipse diameter #:fill (rgb* 'red alpha)))
(define green-circle (geo-ellipse diameter #:fill (rgb* 'green alpha)))
(define blue-circle (geo-ellipse diameter #:fill (rgb* 'blue alpha)))
(define yellow-circle (geo-ellipse 124 #:border yellow-stroke #:fill (rgb* 'yellow 1/2)))
(define 3pc (geo-pin* 1/8 11/48 0 0 (geo-pin* 1/3 0 0 0 red-circle green-circle) blue-circle))
(define sine (geo-rectangular 100 100 build-sine))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  3pc
  (geo-composite green-circle -50 0 blue-circle)
  (geo-pin 3pc 0 0 yellow-circle 64 64)
  (geo-frame (geo-pin* 1/8 1/8 0 0 yellow-circle yellow-circle yellow-circle))
  (geo-frame (geo-frame (geo-pin* -1/8 -1/8 0 0 yellow-circle yellow-circle yellow-circle)))
  (geo-cc-superimpose* (list 3pc yellow-circle))
  
  (geo-pin* 1/5 1/5 0 0 sine sine)
  (geo-pin* 1/2 0 0 0 sine sine)
  (geo-composite #:operator 'screen sine 50 0 sine))
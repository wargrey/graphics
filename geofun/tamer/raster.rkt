#lang typed/racket/base

(require geofun/vector)
(require geofun/bitmap)

(require "flomap.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define g:sine (geo-rectangular 100 100 build-sine))
(define b:sine (bitmap-rectangular 100 100 build-sine))

b:sine
g:sine

(geo-bitmap b:sine)

;(bitmap-save b:sine (build-path (find-system-path 'desk-dir) "bitmap.png"))
;(geo-save #:format 'png g:sine (build-path (find-system-path 'desk-dir) "vector.png"))

#lang typed/racket

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define vertex '(0+0i -40+80i 240 -40-80i))

(geo-frame (geo-polyline #:stroke (desc-stroke #:color 'royalblue #:width 4.0) vertex #:window -1.0-1.0i))

(geo-frame (geo-polygon #:stroke 'green #:fill 'burlywood vertex #:window -1.0-1.0i))

(geo-polygon #:fill 'silver '((0 . 0) (100 . 0)))
(geo-polygon #:fill 'silver '((0 . 0) (100 . 0) (99.5 . 0.5) (0.5 . 0.5)))
(geo-polygon #:fill 'silver '((0 . 0) (100 . 0) (95 . 5) (5 . 5)))

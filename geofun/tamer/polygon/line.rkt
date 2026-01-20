#lang typed/racket

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define vertex '(0+0i -40+80i +nan.0 +nan.0 240 -40-80i))

; also @see ../path/segment.rkt
(geo-frame (geo-polyline #:stroke (desc-stroke #:color 'royalblue #:width 4.0 #:cap 'round) vertex))
(geo-frame (geo-polyline #:stroke (desc-stroke #:color 'royalblue #:width 4.0 #:cap 'round) #:scale -1.0 vertex))
(geo-frame (geo-polygon  #:stroke (desc-stroke #:color 'green #:join 'round) #:fill 'burlywood vertex))

(geo-polygon #:fill 'silver '((0 . 0) (100 . 0)))
(geo-polygon #:fill 'silver '((0 . 0) (100 . 0) (99.5 . 0.5) (0.5 . 0.5)))
(geo-polygon #:fill 'silver '((0 . 0) (100 . 0) (95 . 5) (5 . 5)))

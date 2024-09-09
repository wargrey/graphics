#lang typed/racket

(require geofun/vector)
(require geofun/digitama/dc/edge)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(geo-frame
 (geo-edge (list (cons #\M -128.0+64.0i)
                 (cons #\L 0.0+0.0i)
                 (cons #\l 0.0+64.0i)
                 (cons #\l -28.0+0.0i)
                 (cons #\l 0.0-108.0i)
                 (cons #\l 28.0+0.0i))))

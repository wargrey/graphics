#lang typed/racket/base

(require geofun/vector)
(require bitmap)

(require (only-in geofun/digitama/convert geo-object->stream-bytes))
(require (only-in bitmap/digitama/convert bitmap->stream-bytes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cell.bmp (bitmap-solid 'royalblue 20))
(define cell.geo (geo-square 20))

cell.bmp
(bitmap->stream-bytes cell.bmp 'png)
(bitmap->stream-bytes cell.bmp 'pdf)
(bitmap->stream-bytes cell.bmp 'svg)

cell.geo
(geo-object->stream-bytes cell.geo 'png)
(geo-object->stream-bytes cell.geo 'pdf)
(geo-object->stream-bytes cell.geo 'svg)

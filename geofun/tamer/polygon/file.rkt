#lang racket

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pen (desc-stroke #:width 2.0 #:color 'Crimson))
(define brush 'LightSkyBlue)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(geo-file 64 100 #:angle (~rad 30 'deg) #:stroke pen #:fill brush #:dog-ear-corner 'lt)
(geo-file 64 100 #:angle (~rad 60 'deg) #:stroke pen #:fill brush #:dog-ear-corner 'rt)
(geo-file 64 #:stroke pen #:fill brush #:dog-ear-corner 'lb)
(geo-file 64 #:stroke pen #:fill brush #:dog-ear-corner 'rb)

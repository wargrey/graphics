#lang racket

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pen (desc-stroke #:width 2.0 #:color 'SeaGreen))
(define line-pen (desc-stroke #:width 1.0 #:color 'DodgerBlue))
(define brush 'Azure)

(define lines (range% 20 80 15))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(geo-file 64 100 #:dog-ear-angle (~rad 30 'deg) #:stroke pen #:line-stroke line-pen #:fill brush #:dog-ear-corner 'lt #:lines lines)
(geo-file 64 100 #:dog-ear-angle (~rad 60 'deg) #:stroke pen #:line-stroke line-pen #:fill brush #:dog-ear-corner 'rt #:lines lines)
(geo-file 64 100 #:stroke pen #:line-stroke line-pen #:fill brush #:dog-ear-corner 'rt #:lines lines)
(geo-file 64 #:stroke pen #:line-stroke line-pen #:fill brush #:dog-ear-corner 'lb #:lines lines)
(geo-file 64 #:stroke pen #:line-stroke line-pen #:fill brush #:dog-ear-corner 'rb #:lines lines)

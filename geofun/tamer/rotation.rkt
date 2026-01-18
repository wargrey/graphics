#lang typed/racket/base

(require geofun)
(require geofun/tamer/flomap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plane (geo-rectangular 96 64 xy->argb))
(define dot (geo-circle 2.0 #:fill 'DimGray))

(geo-table 8
           (for/list : (Listof Geo) ([deg (in-range 0 360 15)])
             (geo-frame (geo-cc-superimpose
                         (geo-rt-superimpose (geo-rotate plane deg 'deg)
                                             (geo-text deg))
                         dot))))

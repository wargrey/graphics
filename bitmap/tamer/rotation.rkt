#lang typed/racket/base

(require bitmap)
(require geofun/vector)
(require geofun/tamer/flomap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plane (bitmap-rectangular 96 64 build-sine #:density 2.00))
(define dot (geo-freeze (geo-circle 2.0 #:fill 'DimGray)))

(for/list : (Listof Bitmap) ([deg (in-range 0 360 15)])
  (bitmap-frame (bitmap-cc-superimpose
                 (bitmap-rt-superimpose (bitmap-rotate plane deg 'deg)
                                        (geo-freeze (geo-text deg)))
                 dot)))

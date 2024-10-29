#lang typed/racket/base

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-grid : (-> Geo Geo)
  (lambda [c]    
    (define r (geo-hb-append c c c c c c))
    (geo-vl-append r r r r r r)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (geo-grid (geo-square 20 #:stroke 'black))
  (geo-grid (geo-square 20 #:stroke (desc-stroke #:color 'black #:width 2)))
  (geo-grid (geo-lt-crop (geo-square 20 #:stroke (desc-stroke #:color 'black #:width 2)) 20 20)))

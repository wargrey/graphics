#lang typed/racket

;;; https://www.cairographics.org/operators

(require geofun/vector)

(require geofun/digitama/freeze)
(require geofun/digitama/composite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define src : Geo (geo-circle 64 #:stroke #false #:fill (rgba 0.0 0.0 0.9 0.9)))
(define dest : Geo (geo-rectangle 120 90 #:stroke #false #:fill (rgba 0.7 0.0 0.0 0.9)))

(for/list : (Listof Any) ([op (in-list geo-pin-operators)])
  (geo-freeze (geo-composite (geo-composite #:operator op dest 40.0 30.0 src)
                             2.0 0.0
                             (geo-text op))))

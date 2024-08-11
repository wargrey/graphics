#lang typed/racket

;;; https://www.cairographics.org/operators

(require geofun/vector)
(require geofun/digitama/composite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define src : Geo<%> (geo-rectangle 120 90 #:border transparent #:fill (rgba 0.0 0.0 0.9 0.4)))
(define dest : Geo<%> (geo-rectangle 120 90 #:border transparent #:fill (rgba 0.7 0.0 0.0 0.8)))

(for/list : (Listof Geo<%>) ([op (in-list geo-composition-operators)])
  (geo-composite 'over
                 (geo-composite op dest 40.0 30.0 src)
                 2.0 0.0(geo-text op)))

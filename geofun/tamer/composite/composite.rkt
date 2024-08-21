#lang typed/racket

;;; https://www.cairographics.org/operators

(require geofun/vector)

(require geofun/digitama/freeze)
(require geofun/digitama/composite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define src : Geo (geo-rectangle 120 90 #:border #false #:fill (rgba 0.0 0.0 0.9 0.4)))
(define dest : Geo (geo-rectangle 120 90 #:border #false #:fill (rgba 0.7 0.0 0.0 0.8)))

(define grp : Geo:Group (geo-composite dest 40.0 30.0 src))

(for/list : (Listof Any) ([op (in-list geo-pin-operators)])
  (parameterize ([default-pin-operator op])
    (geo-freeze (geo-composite #:operator 'over grp 2.0 0.0 (geo-text op)))))

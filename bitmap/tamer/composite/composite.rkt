#lang typed/racket

;;; https://www.cairographics.org/operators

(require bitmap)
(require geofun/digitama/composite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define src : Bitmap  (bitmap-rectangle 120 90 #:stroke #false #:fill (rgba 0.0 0.0 0.9 0.4)))
(define dest : Bitmap (bitmap-rectangle 120 90 #:stroke #false #:fill (rgba 0.7 0.0 0.0 0.8)))

(for/list : (Listof Bitmap) ([op (in-list geo-pin-operators)])
  (bitmap-composite #:operator 'over
                    (bitmap-composite dest 40.0 30.0 src #:operator op)
                    2.0 0.0 (bitmap-text op)))

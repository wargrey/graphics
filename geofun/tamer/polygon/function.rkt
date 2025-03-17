#lang typed/racket

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define stroke (desc-stroke #:color 'royalblue #:width 2.0 #:cap 'round))

(define kx : (-> Flonum Flonum)
  (λ [[v : Flonum]]
    (* 0.2 v)))

(define discrete-floor : (-> Flonum (Option Flonum))
  (lambda [x]
    (define i (floor x))
    (define sx (* (- x i) 100.0))

    (when (zero? (floor sx))
      (displayln x))

    (and (not (zero? (floor sx)))
         i)))

(geo-frame (geo-cartesian #:stroke stroke sin +100 -3 +3))
(geo-frame (geo-cartesian #:stroke stroke kx +100-100i -3 +3))
(geo-frame (geo-cartesian #:stroke stroke sin 100 -3 +3 -0.618 +0.618))
(geo-frame (geo-cartesian #:stroke stroke discrete-floor 100 -3 +3))

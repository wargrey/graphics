#lang typed/racket

(require geofun/vector)

(require plotfun/digitama/axis/renderer/function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define stroke (desc-stroke #:width 2.0 #:cap 'round))

(define discrete-floor : (-> Flonum (Option Flonum))
  (lambda [x]
    (define sx (* (- x (round x)) 100.0))

    (when (zero? (floor sx))
      (displayln x))

    (and (not (zero? (floor sx)))
         (floor x))))

(define discrete-ceiling : (-> Flonum (Option Flonum))
  (lambda [x]
    (define sx (* (- x (round x)) 100.0))

    (when (zero? (floor sx))
      (displayln x))

    (and (not (zero? (floor sx)))
         (ceiling x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#;(geo-cc-superimpose
 (plot:function #:scale +100+100i #:stroke 'orange      sin -3 +3)
 (plot:function #:scale +100+100i #:stroke 'crimson     sin -3.0 +3.0 -0.618 +0.618)
 (plot:function #:scale +100-100i #:stroke 'purple      sqr -2 +2)
 (plot:function #:scale +100+100i #:stroke 'forestgreen discrete-floor -3.0 +3.0)
 (plot:function #:scale +100+100i #:stroke 'royalblue   discrete-ceiling -3.0 +3.0))

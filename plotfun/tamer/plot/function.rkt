#lang typed/racket

(require geofun/vector)

(require plotfun/cartesian)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define stroke (desc-stroke #:width 2.0 #:cap 'round))

(define discrete-floor : (-> Real (Option Real))
  (lambda [x]
    (define sx (* (- x (round x)) 100.0))

    (when (zero? (floor sx))
      (displayln (cons 'floor x)))

    (and (not (zero? (floor sx)))
         (floor x))))

(define discrete-ceiling : (-> Real (Option Real))
  (lambda [x]
    (define sx (* (- x (round x)) 100.0))

    (when (zero? (floor sx))
      (displayln (cons 'ceiling x)))

    (and (not (zero? (floor sx)))
         (ceiling x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (plot-cartesian
   (function #:stroke 'forestgreen            discrete-floor -3.0 +3.0)
   (function #:stroke 'royalblue              discrete-ceiling -3.0 +3.0)
   (function #:stroke 'purple                 sqr  -2 +2)
   (function #:stroke 'crimson                sqrt -2 +2))

  (plot-cartesian
   (function #:stroke 'orange      sin -2 +2)
   (function #:stroke 'crimson     sin -3.0 +3.0 -3/5 +3/5)))

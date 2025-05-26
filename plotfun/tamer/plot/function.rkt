#lang typed/racket

(require geofun/vector)
(require plotfun/cartesian)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define discrete-floor : (-> Real (Option Real))
  (lambda [x]
    (define sx (* (- x (round x)) 100.0))

    (and (not (zero? (floor sx)))
         (floor x))))

(define discrete-ceiling : (-> Real (Option Real))
  (lambda [x]
    (define sx (* (- x (round x)) 100.0))

    (and (not (zero? (floor sx)))
         (ceiling x))))

(define normal-dist : (-> Real (Option Real))
  (lambda [x]
    (/ (exp (/ (sqr x) -2.0))
       (sqrt 2pi))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (plot-cartesian
   (function #:stroke (plot-desc-pen #:color 'grey #:dash 'long-dash) values)
   (function #:stroke 'forestgreen discrete-floor -3.0)
   (function #:stroke 'royalblue   discrete-ceiling)
   (function #:stroke 'cyan        cos)
   (function #:stroke 'orange      cos  -1 +1)
   (function #:stroke 'purple      sqr  -2 +2)
   (function #:stroke 'crimson     sqrt +0 +4))

  (plot-cartesian
   (function #:stroke 'crimson     sin -4.0 +4.0 -1 +1)
   (function #:stroke 'orange      sin -3 +3 -4/5 +4/5)
   (function #:stroke 'forestgreen normal-dist)))

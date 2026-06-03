#lang typed/racket/base

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-renamon-rule! R
  #:= [~> [33 #:= R [=> + R] R [=> - R] R]
          [33 #:= R [=> + R] R]
          [33 #:= R [=> - R] R]]
  #:- F)
(define-renamon-generator! stochastic-tree #:order 5 #:angle (~rad 25.7 'deg) #:- R)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (default-border-paint (desc-border #:color (rgb* 'RoyalBlue 0.1)))

  (geo-hb-append* #:gapsize 4.0
                  (for/list : (Listof Geo) ([order (in-range 0 4)])
                    (geo-frame (stochastic-tree*! (make-renamon 10 #:stroke (* (random 360) 1.0)))))))

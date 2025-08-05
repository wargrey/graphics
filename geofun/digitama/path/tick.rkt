#lang typed/racket/base

(provide (all-defined-out))

(require racket/case)

(require "../geometry/footprint.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Tick-Placement (U 'positive 'center 'negative))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-xtick-footprints : (-> Nonnegative-Flonum Flonum Geo-Tick-Placement Geo-Path-Clean-Prints+)
  (lambda [length α placement]
    (define xV (make-polar length α))
    (define yV (* xV 0.0-1.0i))
    (define u (* (/ yV (magnitude yV)) length))
    
    (case/eq placement
      [(positive) (list the-M0 (gpp:point #\L (+ u)))]
      [(negative) (list the-M0 (gpp:point #\L (- u)))]
      [else (list (gpp:point #\M (* u -0.5))
                  (gpp:point #\L (* u +0.5)))])))

(define geo-ytick-footprints : (-> Nonnegative-Flonum Flonum Geo-Tick-Placement Geo-Path-Clean-Prints+)
  (lambda [length α placement]
    (define xV (make-polar length α))
    (define yV (* xV 0.0-1.0i))
    (define u (* (/ xV (magnitude xV)) length))
    
    (case/eq placement
      [(positive) (list the-M0 (gpp:point #\L (+ u)))]
      [(negative) (list the-M0 (gpp:point #\L (- u)))]
      [else (list (gpp:point #\M (* u -0.5))
                  (gpp:point #\L (* u +0.5)))])))

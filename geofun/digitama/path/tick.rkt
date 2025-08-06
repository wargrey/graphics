#lang typed/racket/base

(provide (all-defined-out))

(require racket/case)

(require "../geometry/footprint.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Tick-Placement (U 'positive 'cross 'negative))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-path-tick-footprints : (-> Nonnegative-Flonum Float-Complex Float-Complex Geo-Tick-Placement Geo-Path-Clean-Prints+)
  (lambda [length pos tan-unitV placement]
    (geo-tick-footprints (* tan-unitV 0.0-1.0i length) placement pos)))

(define geo-xtick-footprints : (-> Nonnegative-Flonum Flonum Geo-Tick-Placement Geo-Path-Clean-Prints+)
  (lambda [length α placement]
    (geo-tick-footprints (* (make-polar length α) 0.0-1.0i) placement)))

(define geo-ytick-footprints : (-> Nonnegative-Flonum Flonum Geo-Tick-Placement Geo-Path-Clean-Prints+)
  (lambda [length α placement]
    (geo-tick-footprints (make-polar length α) placement)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-tick-footprints :  (case-> [Float-Complex Geo-Tick-Placement -> Geo-Path-Clean-Prints+]
                                       [Float-Complex Geo-Tick-Placement Float-Complex -> Geo-Path-Clean-Prints+])
  (case-lambda
    [(v placement)
     (case/eq placement
       [(positive) (list the-M0 (gpp:point #\L (+ v)))]
       [(negative) (list the-M0 (gpp:point #\L (- v)))]
       [else (list (gpp:point #\M (* v -0.5))
                   (gpp:point #\L (* v +0.5)))])]
    [(v placement O)
     (case/eq placement
       [(positive) (list (gpp:point #\M O) (gpp:point #\L (+ O v)))]
       [(negative) (list (gpp:point #\M O) (gpp:point #\L (- O v)))]
       [else (list (gpp:point #\M (+ O (* v -0.5)))
                   (gpp:point #\L (+ O (* v +0.5))))])]))

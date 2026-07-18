#lang typed/racket/base

(require plotfun/cartesian)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define experiments : (Listof Complex)
  (list 4+3.16i 8+3.34i 12+3.72i 16+3.92i
        20+3.02i 24+2.80i
        28+2.82i 32+2.82i))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (plot-cartesian #:x-ticks (plot-fixed-ticks (cons 0 (map real-part experiments)))
                  #:x-label "水滴数" #:x-unit-desc "滴" #:x-range 32
                  #:y-label "f" #:y-unit-desc "N" #:y-range 8.0
                  (list (lines experiments))))

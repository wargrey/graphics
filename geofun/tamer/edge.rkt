#lang typed/racket

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define path : (Listof Point2D) (list 0.0 100.0 +50i -64i 50-64i 50))
(define pen : Stroke (desc-stroke #:width 2.0))

(define edge-examplify : (-> Option-Geo-Tip Option-Geo-Tip Geo)
  (lambda [smkr emkr]
    (geo-hc-append #:gapsize 16.0
                   (geo-edge #:stroke (desc-stroke pen #:color 'RoyalBlue)
                             #:tip-placement 'inside
                             #:source-tip smkr #:target-tip emkr
                             path)
                   
                   (geo-edge #:stroke (desc-stroke pen #:color 'ForestGreen)
                             #:tip-placement 'center
                             #:source-tip smkr #:target-tip emkr
                             path)
                   
                   (geo-edge #:stroke (desc-stroke pen #:color 'DodgerBlue)
                             #:tip-placement 'outside
                             #:source-tip smkr #:target-tip emkr
                             path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(edge-examplify default-aggregation-tip default-arrow-tip)
(edge-examplify #false default-generalization-tip)
(edge-examplify default-arrow-tip #false)

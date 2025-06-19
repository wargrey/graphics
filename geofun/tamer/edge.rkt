#lang typed/racket

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define path : (Listof Point2D) (list 0.0 100.0 +50i -64i 50-64i 50))
(define pen : Stroke (desc-stroke #:width 2.0))

(define edge-examplify : (-> (Listof Point2D) Option-Geo-Tip Option-Geo-Tip Geo)
  (lambda [path smkr emkr]
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
(edge-examplify path default-aggregation-tip default-arrow-tip)
(edge-examplify path #false default-generalization-tip)
(edge-examplify path default-arrow-tip #false)
(edge-examplify path default-bullet-tip default-bullet-tip)
(edge-examplify path default-circle-tip default-circle-tip)
(edge-examplify path default-dot-tip default-dot-tip)
(edge-examplify path default-odot-tip default-odot-tip)
(edge-examplify path default-pixel-tip default-pixel-tip)

(edge-examplify (list 0.0+0.0i) default-arrow-tip default-arrow-tip)
(edge-examplify (list 1.0-1.0i) default-arrow-tip default-arrow-tip)

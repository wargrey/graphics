#lang typed/racket

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define path : (Listof Point2D) (list 0.0 100.0 +50i -64i 50-64i 50))
(define pen : Stroke (desc-stroke #:width 2.0))

(define edge-examplify : (-> Option-Geo-Marker Option-Geo-Marker Geo)
  (lambda [smkr emkr]
    (geo-hc-append #:gapsize 16.0
                   (geo-edge #:stroke (desc-stroke pen #:color 'RoyalBlue)
                             #:marker-placement 'inside
                             #:source-marker smkr #:target-marker emkr
                             path)
                   
                   (geo-edge #:stroke (desc-stroke pen #:color 'ForestGreen)
                             #:marker-placement 'center
                             #:source-marker smkr #:target-marker emkr
                             path)
                   
                   (geo-edge #:stroke (desc-stroke pen #:color 'DodgerBlue)
                             #:marker-placement 'outside
                             #:source-marker smkr #:target-marker emkr
                             path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(edge-examplify default-aggregation-marker default-arrow-marker)
(edge-examplify #false default-generalization-marker)
(edge-examplify default-arrow-marker #false)

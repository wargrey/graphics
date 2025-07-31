#lang typed/racket

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define path : (Listof PolyCurve2D) (list 100.0 200.0 (list 175+75i 125+25i 100+50i) 100-64i (list 121-72i 142-64i) 142))
(define pen : Stroke (desc-stroke #:width 2.0))
(define bezier-pen : Stroke (desc-stroke #:width 1.5 #:dash 'long-dash))

(define path-examplify : (->* ((Listof PolyCurve2D) Option-Geo-Tip Option-Geo-Tip Stroke) ((Option String)) Geo)
  (lambda [path smkr emkr pen [label #false]]
    (define glabels
      (and label
           (make-geo-path-labels (map string (string->list label)) 0.25)))

    (geo-hc-append #:gapsize 16.0
                   (geo-path #:stroke (desc-stroke pen #:color 'RoyalBlue)
                             #:tip-placement 'inside
                             #:source-tip smkr #:target-tip emkr
                             #:labels glabels
                             path)
                   
                   (geo-path #:stroke (desc-stroke pen #:color 'ForestGreen)
                             #:tip-placement 'center
                             #:source-tip smkr #:target-tip emkr
                             #:labels glabels
                             path)
                   
                   (geo-path #:stroke (desc-stroke pen #:color 'DodgerBlue)
                             #:tip-placement 'outside
                             #:source-tip smkr #:target-tip emkr
                             #:labels glabels
                             path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (path-examplify path default-aggregation-tip default-arrow-tip pen)
  (path-examplify path #false default-generalization-tip pen)
  (path-examplify path default-arrow-tip #false pen)
  (path-examplify path default-bullet-tip default-bullet-tip pen)
  (path-examplify path default-circle-tip default-circle-tip pen)
  (path-examplify path default-dot-tip default-dot-tip pen)
  (path-examplify path default-odot-tip default-odot-tip pen)
  (path-examplify path default-pixel-tip default-pixel-tip pen)
  
  (path-examplify (list 0.0+0.0i) default-arrow-tip default-arrow-tip pen)
  (path-examplify (list 1.0-1.0i) default-arrow-tip default-arrow-tip pen)
  
  (path-examplify (list (list 220+60i 20+110i 70+250i))
                  default-bullet-tip default-generalization-tip
                  bezier-pen "reizeb citardauq")

  (path-examplify (list (list 25+128i 102.4+230.4i 153.6+25.6i 230.4+128i))
                  default-bullet-tip default-composition-tip
                  bezier-pen "cubic bezier")
  
  (path-examplify (list (list 198+18i   34+57i 18+156i 221+90i
                              186+177i   14+82i 12+236i 45+290i
                              218+294i 248+188i))
                  default-aggregation-tip default-arrow-tip
                  bezier-pen "high-order bezier"))

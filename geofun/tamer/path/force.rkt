#lang typed/racket

(require geofun/vector)
(require geofun/markup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define path : (Listof PolyCurve2D) (list 100.0 200.0 (list 175+75i 125+25i 100+50i) 100-64i (list 121-72i 142-64i) 142))
(define font : Font (desc-font #:family 'math #:size 'large))
(define pen : Stroke (desc-stroke #:width 1.5))
(define tick-pen : Stroke (desc-stroke #:width 1.0 #:color 'DimGray))
(define aux-pen : Stroke (desc-stroke #:width 0.5 #:dash 'long-dash #:color 'DimGray))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define F1 : Complex 320)
  (define F2 : Complex -440i)
  (define scale-step : Index 100)
  
  (geo-path-group (geo-path (list F1 (+ F1 F2) F2) #:stroke aux-pen)
                  (geo-path #:source-tip default-bullet-tip #:target-tip default-arrow-tip #:target-placement 'inside
                            #:stroke (desc-stroke pen #:color 'RoyalBlue)
                            #:labels (make-geo-path-label (<span> null "F" (<sub> "1")) 1.0 #:rotate? #false #:distance '(-75 %) #:font font)
                            (list 0 F1) scale-step)
                  (geo-path #:source-tip default-bullet-tip #:target-tip default-arrow-tip #:target-placement 'inside #:tick-placement 'negative
                            #:stroke (desc-stroke pen #:color 'ForestGreen)
                            #:labels (make-geo-path-label (<span> null "F" (<sub> "2")) 1.0 #:rotate? #false #:distance '(+75 %) #:font font)
                            (list 0 F2) scale-step)
                  (geo-path #:source-tip default-bullet-tip #:target-tip default-arrow-tip #:target-placement 'inside
                            #:stroke (desc-stroke pen #:color 'Purple)
                            #:labels (list (make-geo-path-label "O" -0.03 #:rotate? #false #:font font #:distance 0.0)
                                           (make-geo-path-label "F" +1.00 #:rotate? #false #:font font))
                            (list 0 (+ F1 F2)) scale-step)))

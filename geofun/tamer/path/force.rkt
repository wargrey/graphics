#lang typed/racket

(require geofun/vector)
(require geofun/markup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define path : (Listof PolyCurve2D) (list 100.0 200.0 (list 175+75i 125+25i 100+50i) 100-64i (list 121-72i 142-64i) 142))
(define pen : Stroke (desc-stroke #:width 1.0))
(define aux-pen : Stroke (desc-stroke #:width 0.5 #:dash 'long-dash #:color 'DimGray))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define F1 : Complex 16-96i)
  (define F2 : Complex 64)
  
  (geo-path-group (geo-path (list F1 (+ F1 F2) F2) #:stroke aux-pen)
                  (geo-cc-superimpose (geo-path #:source-tip default-bullet-tip #:target-tip default-arrow-tip #:target-placement 'inside
                                                #:stroke (desc-stroke pen #:color 'RoyalBlue)
                                                #:labels (make-geo-path-label (<span> null "F" (<sub> "1")) 1.0 #:rotate? #false #:distance '(+75 %))
                                                (list 0 F1))
                                      (geo-path #:source-tip default-bullet-tip #:target-tip default-arrow-tip #:target-placement 'inside
                                                #:stroke (desc-stroke pen #:color 'ForestGreen)
                                                #:labels (make-geo-path-label (<span> null "F" (<sub> "2")) 1.0 #:rotate? #false #:distance '(-75 %))
                                                (list 0 F2)))
                  (geo-path #:source-tip default-bullet-tip #:target-tip default-arrow-tip #:target-placement 'inside
                            #:stroke (desc-stroke pen #:color 'Purple)
                            #:labels (make-geo-path-label "F" 1.0 #:rotate? #false)
                            (list 0 (+ F1 F2)))))

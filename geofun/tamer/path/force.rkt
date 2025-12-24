#lang typed/racket

(require geofun/vector)
(require geofun/markup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define font : Font (desc-font #:family 'math #:size 'large))
(define pen : Pen (desc-stroke #:width 1.5))
(define aux-pen : Pen (desc-stroke #:width 0.5 #:dash 'long-dash #:color 'DimGray #:opacity 0.32))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define F1 : Complex 160+32i)
  (define F2 : Complex -18-220i)
  (define scale-step : Index 50)

  (geo-vc-append
   (geo-path #:source-tip default-pixel-tip #:target-tip default-pixel-tip
             #:stroke (desc-stroke pen #:color 'DimGray)
             #:labels (make-geo-path-label "10N" 0.5 #:font font)
             #:skip-tick0? #false
             (list 0 scale-step) scale-step)
   
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
                             #:stroke (desc-stroke pen #:color 'Purple #:opacity 0.32)
                             #:labels (list (make-geo-path-label "O" -0.05 #:rotate? #false #:font font #:distance 0.0)
                                            (make-geo-path-label "F" +1.00 #:rotate? #false #:font font))
                             (list 0 (+ F1 F2)) scale-step))))
  
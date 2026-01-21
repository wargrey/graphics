#lang racket

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define border-pen (desc-stroke #:width 4.0 #:color 'RoyalBlue #:opacity 0.5))
(define line-pen (desc-stroke #:width 1.0 #:color 'DodgerBlue #:opacity 0.5))
(define brush (desc-brush #:color 'Azure #:opacity 0.5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(geo-open-rectangle #:stroke border-pen #:line-stroke line-pen #:fill brush
                    #:hlines (range% 10 100 10) #:hline-span (&% 90)
                    #:open-sides '(t b)
                    128 (&% 161.8))

(geo-open-square* #:stroke border-pen #:line-stroke line-pen #:fill brush
                  #:hlines (range% 10 100 10) #:hline-span (&% 85)
                  #:top-open-span (&% 80) #:bottom-open-span (&% 80)
                  #:right-open-span (&% 80) #:left-open-span (&% 80)
                  128)

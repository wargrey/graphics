#lang racket

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define border-pen (desc-stroke #:width 4.0 #:color 'RoyalBlue #:opacity 0.5))
(define line-pen (desc-stroke #:width 1.0 #:color 'DodgerBlue #:opacity 0.5))
(define brush (desc-brush #:color 'Azure #:opacity 0.5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(geo-open-rectangle #:stroke border-pen #:line-stroke line-pen #:fill brush
                    #:open-sides '(r)
                    128 (&% 61.8))

(geo-open-square #:stroke border-pen #:line-stroke line-pen #:fill brush
                 #:hlines (range% 10 100 10) #:hline-span (&% 85)
                 #:open-sides (make-geo-open-sides #:top-span (&% 80) #:bottom-span (&% 80)
                                                   #:right-span (&% 80) #:left-span (&% 80))
                 128)

(geo-hc-append #:gapsize 16.0
               (geo-open-square #:stroke border-pen #:line-stroke line-pen #:fill brush
                                #:hlines (range% 10 100 10) #:hline-span (&% 85)
                                #:open-sides (make-geo-open-sides #:top-span (&% 90) #:bottom-span (&% 90)
                                                                  #:top-pos% 1.0 #:bottom-pos% 1.0
                                                                  #:left-span 0)
                                128)
               
               (geo-open-square #:stroke border-pen #:line-stroke line-pen #:fill brush
                                #:hlines (range% 10 100 10) #:hline-span (&% 85)
                                #:open-sides (make-geo-open-sides #:top-span (&% 90) #:bottom-span (&% 90)
                                                                  #:top-pos% 0.0 #:bottom-pos% 0.0
                                                                  #:right-span 0)
                                 128))

(geo-vc-append #:gapsize 16.0
               (geo-open-square #:stroke border-pen #:line-stroke line-pen #:fill brush
                                #:hlines (range% 10 100 10) #:hline-span (&% 85)
                                #:open-sides (make-geo-open-sides #:left-span (&% 90) #:right-span (&% 90)
                                                                  #:left-pos% 1.0 #:right-pos% 1.0
                                                                  #:top-span 0)
                                128)
               
               (geo-open-square #:stroke border-pen #:line-stroke line-pen #:fill brush
                                #:hlines (range% 10 100 10) #:hline-span (&% 85)
                                #:open-sides (make-geo-open-sides #:left-span (&% 90) #:right-span (&% 90)
                                                                  #:left-pos% 0.0 #:right-pos% 0.0
                                                                  #:bottom-span 0)
                                128))

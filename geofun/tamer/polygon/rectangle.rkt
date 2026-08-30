#lang racket

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define red-pen (desc-stroke #:width 2.0 #:color 'Crimson #:opacity 0.5))
(define green-pen (desc-stroke red-pen #:color 'green))
(define blue-pen (desc-stroke red-pen #:color 'blue))
(define brush (desc-brush #:color 'Snow #:opacity 0.5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(geo-rounded-square 128 16 #:vlines (range% 10 100 10) #:hlines (range% -10 -100 -10) #:stroke 'RoyalBlue)

(geo-hc-append #:gapsize 16.0
               (geo-rounded-square 128 16 #:vlines (list 1 2 4 8 16 32 64) #:hlines (list 0.5 -0.5))
               (geo-rounded-square #:vlines (list 1/2 1 2 4 8 16 32 -32 -8 -4 -2 -1 -1/2)
                                   #:hlines (list 1/2 1 2 4 8 16 32 -32 -8 -4 -2 -1 -1/2)
                                   128 -16)

               (geo-chamfered-square #:vlines (list 1/2 1 2 4 8 16 32 -32 -8 -4 -2 -1 -1/2)
                                     #:hlines (list 1/2 1 2 4 8 16 32 -32 -8 -4 -2 -1 -1/2)
                                     128 16)

               (geo-rounded-square #:vlines (list 4 8 16 32)
                                   #:hlines (list 4 8 16 32)
                                   #:exclude-corners '(rt lb)
                                   128 32)
               
               (geo-rounded-square #:vlines (list -32 -16 -8 -4)
                                   #:hlines (list -32 -16 -8 -4)
                                   #:exclude-corners '(lt rb)
                                   128 -32)

               (geo-chamfered-square #:vlines (list 1/2 1 2 4 8 16 32 -32 -8 -4 -2 -1 -1/2)
                                     #:hlines (list 1/2 1 2 4 8 16 32 -32 -8 -4 -2 -1 -1/2)
                                     #:exclude-corners '(lt rb)
                                     128 16))

(geo-hc-append #:gapsize 8.0
               (geo-rounded-rectangle 256 64 (&:  phi) #:exclude-corners '(rt rb lb lt) #:stroke red-pen #:fill brush)
               (geo-rounded-rectangle 256 64 (&: -phi) #:exclude-corners '(rt rb lb lt) #:stroke red-pen #:fill brush)
               (geo-chamfered-rectangle 256 64 (&: phi) #:exclude-corners '(rt rb lb lt) #:stroke red-pen #:fill brush))

(geo-hc-append #:gapsize 8.0
               (geo-rounded-rectangle 256 64 (&:  phi) #:exclude-corners '(rt lb) #:stroke red-pen #:fill brush)
               (geo-rounded-rectangle 256 64 (&:  phi) #:exclude-corners '(lt rb) #:stroke red-pen #:fill brush)
               (geo-rounded-rectangle 256 64 (&:  phi) #:exclude-corners '(lt rt lb) #:stroke red-pen #:fill brush)
               (geo-rounded-rectangle 256 64 (&:  phi) #:exclude-corners '(rt rb lb) #:stroke red-pen #:fill brush))

(geo-hc-append #:gapsize 8.0
               (geo-rounded-rectangle 256 64 (&: -phi) #:exclude-corners '(rt lb) #:stroke green-pen #:fill brush)
               (geo-rounded-rectangle 256 64 (&: -phi) #:exclude-corners '(lt rb) #:stroke green-pen #:fill brush)
               (geo-rounded-rectangle 256 64 (&: -phi) #:exclude-corners '(lt rt lb) #:stroke green-pen #:fill brush)
               (geo-rounded-rectangle 256 64 (&: -phi) #:exclude-corners '(rt rb lb) #:stroke green-pen #:fill brush))

(geo-hc-append #:gapsize 8.0
               (geo-chamfered-rectangle 256 64 (&: phi) #:exclude-corners '(rt lb) #:stroke blue-pen #:fill brush)
               (geo-chamfered-rectangle 256 64 (&: phi) #:exclude-corners '(lt rb) #:stroke blue-pen #:fill brush)
               (geo-chamfered-rectangle 256 64 (&: phi) #:exclude-corners '(lt rt lb) #:stroke blue-pen #:fill brush)
               (geo-chamfered-rectangle 256 64 (&: phi) #:exclude-corners '(rt rb lb) #:stroke blue-pen #:fill brush))

(geo-cc-superimpose
 (geo-chamfered-rectangle 128 256 (&: 1/phi) 30 'deg #:fill brush #:stroke red-pen)
 (geo-chamfered-rectangle 128 256 (&: 1/phi) 60 'deg #:fill brush #:stroke green-pen)
 (geo-chamfered-rectangle 128 256 (&: 1/phi) 90 'deg #:fill brush #:stroke blue-pen))

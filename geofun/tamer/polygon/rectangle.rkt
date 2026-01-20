#lang racket

(require digimon/measure)
(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pen (desc-stroke #:width 2.0 #:color 'Crimson))
(define brush 'Snow)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(geo-rounded-square 128 16 #:vlines (range% 10 100 10) #:hlines (range% -10 -100 -10) #:stroke 'RoyalBlue)

(geo-rounded-square 128 16 #:vlines (list 1 2 4 8 16 32 64) #:hlines (list 0.5 -0.5))
(geo-rounded-square #:vlines (list 1/2 1 2 4 8 16 32 -32 -8 -4 -2 -1 -1/2)
                    #:hlines (list 1/2 1 2 4 8 16 32 -32 -8 -4 -2 -1 -1/2)
                    128 -16)

(geo-rounded-square #:vlines (list 4 8 16 32)
                    #:hlines (list 4 8 16 32)
                    #:exclude-corners '(rt lb)
                    128 32)

(geo-rounded-square #:vlines (list -32 -16 -8 -4)
                    #:hlines (list -32 -16 -8 -4)
                    #:exclude-corners '(lt rb)
                    128 -32)

(geo-rounded-rectangle 256 64 (&:  phi) #:exclude-corners '(rt lb) #:stroke pen #:fill brush)
(geo-rounded-rectangle 256 64 (&:  phi) #:exclude-corners '(lt rb) #:stroke pen #:fill brush)
(geo-rounded-rectangle 256 64 (&:  phi) #:exclude-corners '(lt rt lb) #:stroke pen #:fill brush)
(geo-rounded-rectangle 256 64 (&:  phi) #:exclude-corners '(rt rb lb) #:stroke pen #:fill brush)
(geo-rounded-rectangle 256 64 (&:  phi) #:exclude-corners '(rt rb lb lt) #:stroke pen #:fill brush)

(geo-rounded-rectangle 256 64 (&: -phi) #:exclude-corners '(rt lb) #:stroke pen #:fill brush)
(geo-rounded-rectangle 256 64 (&: -phi) #:exclude-corners '(lt rb) #:stroke pen #:fill brush)
(geo-rounded-rectangle 256 64 (&: -phi) #:exclude-corners '(lt rt lb) #:stroke pen #:fill brush)
(geo-rounded-rectangle 256 64 (&: -phi) #:exclude-corners '(rt rb lb) #:stroke pen #:fill brush)
(geo-rounded-rectangle 256 64 (&: -phi) #:exclude-corners '(rt rb lb lt) #:stroke pen #:fill brush)

(geo-chamfered-square #:vlines (list 1/2 1 2 4 8 16 32 -32 -8 -4 -2 -1 -1/2)
                      #:hlines (list 1/2 1 2 4 8 16 32 -32 -8 -4 -2 -1 -1/2)
                      128 16)

(geo-chamfered-rectangle 256 64 (&: phi) #:exclude-corners '(rt lb) #:stroke pen #:fill brush)
(geo-chamfered-rectangle 256 64 (&: phi) #:exclude-corners '(lt rb) #:stroke pen #:fill brush)
(geo-chamfered-rectangle 256 64 (&: phi) #:exclude-corners '(lt rt lb) #:stroke pen #:fill brush)
(geo-chamfered-rectangle 256 64 (&: phi) #:exclude-corners '(rt rb lb) #:stroke pen #:fill brush)
(geo-chamfered-rectangle 256 64 (&: phi) #:exclude-corners '(rt rb lb lt) #:stroke pen #:fill brush)

(geo-chamfered-rectangle 50 150 (&: phi) 90 'deg #:stroke pen #:fill brush)

#lang typed/racket/base

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define vertices '(0+0i -40+80i +nan.0 +nan.0 240 -40-80i))

(geo-polycurve* #:stroke (desc-stroke #:color 'royalblue #:width 4.0 #:cap 'round) #:fill 'Azure vertices)
(geo-polycurve* #:stroke (desc-stroke #:color 'royalblue #:width 4.0 #:cap 'round) #:fill 'Azure #:scale -1.0 vertices)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define stair-vertices '(0+40i 40+40i 40+80i 80+80i 80+120i 120+120i 120+160i))

(geo-polycurve* #:stroke (desc-stroke #:color 'red #:width 4.0 #:cap 'round) #:fill 'yellow stair-vertices)
(geo-polycurve* #:stroke (desc-stroke #:color 'red #:width 4.0 #:cap 'round) #:fill 'yellow #:scale -1.0 stair-vertices)

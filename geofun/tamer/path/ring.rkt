#lang typed/racket/base

(require geofun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pen (desc-stroke #:width 8.0 #:color 'Crimson #:opacity 0.64))
(define brush (desc-brush #:color 'LightSkyBlue #:opacity 0.64))
(define eye-pen (desc-stroke pen #:color 'RoyalBlue))
(define eye-brush (desc-brush brush #:color 'Yellow))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(geo-bullseye* #:eye-stroke eye-pen #:eye-fill eye-brush
               #:stroke pen #:fill brush
               128.0 (&% 20) (range% 40 100 20))

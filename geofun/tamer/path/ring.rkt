#lang typed/racket/base

(require geofun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pen (desc-stroke #:width 2.0 #:color 'RoyalBlue #:opacity 0.64))
(define brush (desc-brush #:color 'PaleGreen #:opacity 0.80))
(define eye-pen (desc-stroke pen #:color 'Crimson))
(define eye-brush (desc-brush brush #:color 'Yellow))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(geo-bullseye #:eye-stroke eye-pen #:eye-fill eye-brush
              #:stroke pen #:fill brush
              64.0 (&% 25) (&% 50) (&% 75))

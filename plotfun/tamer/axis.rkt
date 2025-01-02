#lang typed/racket/base

(require geofun/color)
(require geofun/resize)
(require geofun/constructor)
(require plotfun/axis)

(require racket/list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define timeline-sticker : Plot-Axis-Real->Sticker
   (lambda [id r datum font axis-color]
     (geo-text datum font #:color (rgb* 'black (/ (+ r 1.0) 10.0)))))

(define timeline
  (plot-axis #:tick-range (cons 0 9) #:reals (range 10) #:real->sticker timeline-sticker
             360 0.0))



(module+ main
  timeline)

#lang typed/racket/base

(require geofun/vector)
(require geofun/palette)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-display-lightness : (-> Color Geo)
  (lambda [c]
    (geo-hc-append* (reverse (for/list : (Listof Geo) ([scale (in-range 10 100)])
                               (define s (max 0.0 (* scale 0.01)))
                               (geo-vl-append (if (zero? (remainder scale 5)) (geo-text scale) (geo-text " "))
                                              (geo-rectangle 16 16 #:stroke #false #:fill (oklch-modulate c #:lightness s))
                                              (geo-rectangle 16 16 #:stroke #false #:fill c)
                                              (geo-rectangle 16 16 #:stroke #false #:fill (rgb-scale c s))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (geo-display-lightness (cdr (the-plot-oklch-palette 0)))
  (geo-display-lightness (cdr (the-plot-oklch-palette 1)))
  (geo-display-lightness (cdr (the-plot-oklch-palette 2)))
  (geo-display-lightness (cdr (the-plot-oklch-palette 3)))
  (geo-display-lightness (cdr (the-plot-oklch-palette 4)))
  (geo-display-lightness (cdr (the-plot-oklch-palette 5)))
  (geo-display-lightness (cdr (the-plot-oklch-palette 6))))
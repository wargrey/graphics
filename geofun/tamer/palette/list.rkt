#lang typed/racket/base

(require geofun/vector)
(require geofun/palette)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-display-discrete-palette : (-> Palette-Index->Colors Index Geo)
  (lambda [palette n]
    (define thickness 2.0)

    (define g
      (geo-hc-append* (for/list : (Listof Geo) ([idx (in-range n)])
                        (define cs (palette (max idx 0)))
                        (geo-cc-superimpose (geo-rectangle (/ 640 n) 32 #:fill (cdr cs) #:stroke #false)
                                            (geo-hline (/ 320 n) 1 #:stroke (desc-stroke #:color (car cs) #:width 2.0))))))

    (geo-hc-append #:gapsize 16.0
                   g (geo-text (object-name palette)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (geo-display-discrete-palette the-dark2-palette 8)
  (geo-display-discrete-palette the-paired-palette 12)
  (geo-display-discrete-palette the-pastel1-palette 9)
  (geo-display-discrete-palette the-pastel2-palette 8)
  (geo-display-discrete-palette the-set1-palette 9)
  (geo-display-discrete-palette the-set2-palette 8)
  (geo-display-discrete-palette the-set3-palette 12)
  (geo-display-discrete-palette the-tab10-palette 10)
  (geo-display-discrete-palette the-tab10n-palette 10)
  (geo-display-discrete-palette the-tab20-palette 20)
  (geo-display-discrete-palette the-tab20b-palette 20)
  (geo-display-discrete-palette the-tab20c-palette 20)
  (geo-display-discrete-palette the-plot-oklch-palette 20))

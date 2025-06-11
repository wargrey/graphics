#lang typed/racket/base

(require geofun/vector)
(require colorspace/palette)

(require "misc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-display-hcl : (-> (Option FlRGBA) Geo)
  (lambda [bg]
    (define golden-hcl-palette (oklch-palette-create))
    (define thickness 2.0)

    (define palette
      (geo-hc-append* #:gapsize (* thickness 2.0)
                      (let gen : (Listof Geo) ([idx : Positive-Fixnum 1]
                                               [lines : (Listof Geo) null])
                        (if (<= idx 64)
                            (gen (+ idx 1)
                                 (let ([cs (golden-hcl-palette idx bg)])
                                   (cons (geo-vl-append #:gapsize thickness
                                                        (geo-rectangle 16 64 #:stroke #false #:fill (car cs))
                                                        (geo-rectangle 16 16 #:stroke (desc-stroke #:color (car cs) #:width thickness) #:fill (cdr cs))
                                                        (geo-rectangle 16 64 #:stroke #false #:fill (cdr cs)))
                                         lines)))
                            (reverse lines)))))

    (if (and bg)
        (let-values ([(w h) (geo-size palette)]
                     [(margin) (* thickness 8.0)])
          (geo-cc-superimpose (geo-rectangle (+ w margin) (+ h margin) #:stroke #false #:fill bg)
                              palette))
        palette)))

(geo-display-hcl white)
(geo-display-hcl black)
(geo-display-hcl (rgba 0.6 0.6 0.6 1.0))
(geo-display-hcl (rgba 0.4 0.4 0.4 1.0))

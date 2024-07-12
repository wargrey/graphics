#lang typed/racket/base

(provide (all-defined-out))

(require bitmap)
(require bitmap/projection)

(require racket/math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bacteriophage-logo : (->* (Real) (#:sheath-length Real #:perspective-alpha Real) Bitmap)
  (lambda [R #:sheath-length [sheath-length -1.618] #:perspective-alpha [perspective-alpha 0.618]]
    (define thickness : Real (/ R 32.0))
    (define-values (Rdart Rcollar r) (values (* R 0.618) (* R 0.5 0.618) (* R 0.2)))
    (define edge-stroke (desc-stroke #:color 'SkyBlue #:width thickness))
    (define head-stroke (desc-stroke #:color 'DeepSkyBlue #:width (* thickness 2.0)))
    (define node-border-stroke (desc-stroke #:width (* thickness 2.0)))
    (define node-stroke (desc-stroke #:color (rgb* 'Snow perspective-alpha) #:width (* thickness 1.0)))
    (define fibre-stroke (desc-stroke #:color 'Teal #:width (* thickness 2.0)))
    (define body-color : Color 'SteelBlue)
    (define perspective-color : Color (rgb* 'MintCream perspective-alpha))
    
    (define text (bitmap-text "λ" (desc-font #:size (* R 1.4) #:family "Linux Biolinum Shadow, Bold") #:color 'Crimson))
    
    (define protein-coat
      (bitmap-cc-superimpose
       (bitmap-icosahedron-side-projection R 'edge #:edge #false #:border (desc-stroke head-stroke #:color 'DodgerBlue) #:fill perspective-color)
       text
       (bitmap-icosahedron-side-projection R 'vertex #:edge edge-stroke #:border head-stroke)))

    (define body
      (bitmap-vc-append #:gapsize (* Rcollar -0.618)
                        (bitmap-dart Rcollar 90.0 #:radian? #false #:fill body-color #:border head-stroke #:wing-angle 144.0)
                        (bitmap-arrow Rdart sheath-length 90.0 #:radian? #false #:fill body-color #:shaft-thickness -0.618 #:wing-angle 180.0)))

    (define fibre
      (let* ([fdx Rdart]
             [fdy (/ fdx (sin (degrees->radians 54.0)))])
        (bitmap-polyline (list (cons 0.0 fdy) (cons fdx 0.0) (cons (* fdx 3.0) 0.0) (cons (* fdx 4.0) fdy)) #:stroke fibre-stroke)))

    (define-values (stx-lnode stx-rnode)
      (values
       (bitmap-icosahedron-over-projection #:border (desc-stroke node-border-stroke #:color 'Blue) #:edge node-stroke #:fill perspective-color r 'face)
       (bitmap-icosahedron-over-projection #:border (desc-stroke node-border-stroke #:color 'Lime) #:edge node-stroke #:fill perspective-color r 'face)))

    (bitmap-vc-append #:gapsize (- (* Rdart -1.0) (* (stroke-width fibre-stroke) 1.0))
                      (bitmap-vc-append #:gapsize (* Rcollar -1.618) protein-coat body)
                      (bitmap-pin* 1.0 1.0 0.36 1.0
                                   (bitmap-pin* 0.0 1.0 0.64 0.28 fibre stx-lnode)
                                   stx-rnode))))




(module+ main
  (bacteriophage-logo 2.0)
  (bacteriophage-logo 8.0)
  (bacteriophage-logo 16.0)
  (bacteriophage-logo 32.0)
  (bacteriophage-logo 64.0)
  (bacteriophage-logo 128.0)

  (bitmap-frame (bacteriophage-logo 128.0) #:fill 'ForestGreen))
  
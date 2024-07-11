#lang typed/racket/base

(provide (all-defined-out))

(require bitmap)
(require bitmap/projection)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bacteriophage-logo : (->* (Real) (#:sheath-length Real) Bitmap)
  (lambda [R #:sheath-length [sheath-length -1.618]]
    (define thickness : Real (/ R 32.0))
    (define-values (Rdart Rcollar r) (values (* R 0.618) (* R 0.5 0.618) (* R 0.2)))
    (define edge-stroke (desc-stroke #:color 'SkyBlue #:width thickness))
    (define head-stroke (desc-stroke #:color 'DeepSkyBlue #:width (* thickness 2.0)))
    (define node-stroke (desc-stroke #:color 'Snow #:width (* thickness 2.0)))
    (define fibre-stroke (desc-stroke #:color 'Teal #:width (* thickness 2.0)))
    (define body-color : Color 'SteelBlue)
    (define bg-color : Color 'MintCream)
    
    (define text (bitmap-text "λ" (desc-font #:size (* R 1.4) #:family "Linux Biolinum Shadow, Bold") #:color 'Crimson))
    
    (define protein-coat
      (bitmap-cc-superimpose
       (bitmap-icosahedron-side-projection R 'edge #:edge #false #:border (desc-stroke head-stroke #:color 'DodgerBlue) #:fill bg-color)
       text
       (bitmap-icosahedron-side-projection R 'vertex #:edge edge-stroke #:border head-stroke)))

    (define body
      (bitmap-vc-append #:gapsize (* Rcollar -0.618)
                        (bitmap-dart Rcollar 90.0 #:radian? #false #:fill body-color #:border head-stroke #:wing-angle 144.0)
                        (bitmap-arrow Rdart sheath-length 90.0 #:radian? #false #:fill body-color #:shaft-thickness -0.618 #:wing-angle 180.0)))

    (define fibre
      (let* ([fdx Rdart]
             [fdy (* fdx (sqrt 3.0))])
        (bitmap-polyline (list (cons 0.0 fdy) (cons fdx 0.0) (cons (* fdx 3.0) 0.0) (cons (* fdx 4.0) fdy)) #:stroke fibre-stroke)))

    (define stx-lnode (bitmap-icosahedron-over-projection r 'face #:edge node-stroke #:border (desc-stroke node-stroke #:color 'Blue) #:fill bg-color))
    (define stx-rnode (bitmap-icosahedron-over-projection r 'face #:edge node-stroke #:border (desc-stroke node-stroke #:color 'Lime) #:fill bg-color))

    (bitmap-vc-append #:gapsize (- (* Rdart -1.0) (* (stroke-width fibre-stroke) 1.0))
                      (bitmap-vc-append #:gapsize (* Rcollar -1.618) protein-coat body)
                      (bitmap-pin* 1.0 1.0 0.5 1.0
                                   (bitmap-pin* 0.0 1.0 0.5 0.5 fibre stx-lnode)
                                   stx-rnode))))




(module+ main
  (bacteriophage-logo 2.0)
  (bacteriophage-logo 8.0)
  (bacteriophage-logo 16.0)
  (bacteriophage-logo 32.0)
  (bacteriophage-logo 64.0)
  (bacteriophage-logo 128.0))

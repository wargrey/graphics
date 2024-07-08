#lang typed/racket/base

(require bitmap)
(require bitmap/projection)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bacteriophage-logo : (-> Flonum Bitmap)
  (lambda [R]
    (define-values (Rdart Rcollar r) (values (* R 0.618) (* R 0.5 0.618) (* R 0.2)))
    (define edge-stroke (desc-stroke #:color 'SkyBlue #:width 1))
    (define head-stroke (desc-stroke #:color 'DeepSkyBlue #:width 2))
    (define node-stroke (desc-stroke #:color 'Snow #:width 2))
    (define body-color : Color 'SteelBlue)
    (define fibre-color : Color 'Teal)
    (define bg-color : Color 'MintCream)
    (define fibre-thickness : Flonum 2.0)
    (define fibre-xlen : Flonum (* R 0.618))
    (define fibre-ylen : Flonum (* fibre-xlen (sqrt 3.0)))
    
    (define text (bitmap-text "λ" (desc-font #:size (* R 1.4) #:family "Linux Biolinum Shadow, Bold") #:color 'Crimson))
    
    (define protein-coat
      (bitmap-cc-superimpose
       (bitmap-icosahedron-side-projection R 'edge #:edge #false #:border (desc-stroke head-stroke #:color 'DodgerBlue) #:fill bg-color)
       text
       (bitmap-icosahedron-side-projection R 'vertex #:edge edge-stroke #:border head-stroke)))

    (define body
      (bitmap-vc-append #:gapsize (* Rcollar -0.618)
                        (bitmap-arrowhead Rcollar 90.0 #:radian? #false #:fill body-color #:border head-stroke #:wing-angle 144.0)
                        (bitmap-arrow Rdart R 90.0 #:radian? #false #:fill body-color #:shaft-thickness Rcollar #:wing-angle 180.0)))

    (define fibre
      (bitmap-polyline #:stroke (desc-stroke #:width fibre-thickness #:color fibre-color)
                       (list (cons 0.0 fibre-ylen) (cons fibre-xlen 0.0) (cons (+ fibre-xlen Rdart Rdart) 0.0)
                             (cons (+ fibre-xlen fibre-xlen Rdart Rdart) fibre-ylen))))

    (define stx-lnode (bitmap-icosahedron-over-projection r 'face #:edge node-stroke #:border (desc-stroke node-stroke #:color 'Blue) #:fill bg-color))
    (define stx-rnode (bitmap-icosahedron-over-projection r 'face #:edge node-stroke #:border (desc-stroke node-stroke #:color 'Lime) #:fill bg-color))
    
    (bitmap-vc-append #:gapsize (- (* Rdart -1.0) (* fibre-thickness 1.0))
                      (bitmap-vc-append #:gapsize (* Rcollar -1.618) protein-coat body)
                      (bitmap-pin* 1.0 1.0 0.5 1.0
                                   (bitmap-pin* 0.0 1.0 0.5 0.5 fibre stx-lnode)
                                   stx-rnode))))




(module+ main
  (bacteriophage-logo 64.0))

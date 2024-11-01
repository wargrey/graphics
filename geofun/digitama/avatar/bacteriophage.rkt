#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require geofun/vector)
(require geofun/projection)
(require geofun/digitama/geometry/constants)
(require geofun/digitama/unsafe/dc/icosahedron)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bacteriophage-logo : (->* (Real)
                                  (#:sheath-length Real #:λ-color Color #:fibre-color Color #:fill-color Color #:fill-alpha Real
                                   #:border-color Color #:edge-color Color #:edge-alpha Real
                                   #:head-color (Option Color) #:head-alpha (Option Real) #:tail-color (Option Color) #:tail-alpha (Option Real)
                                   #:feet-fill-color (Option Color) #:feet-alpha (Option Real)
                                   #:left-foot-border-color (Option Color) #:right-foot-border-color (Option Color) #:feet-rotation Real)
                                  Geo)
  (lambda [#:sheath-length [sheath-length -1.618] #:λ-color [λ-color 'Crimson] #:fibre-color [fibre-color 'Teal]
           #:fill-color [fill-color 'MintCream] #:fill-alpha [fill-alpha 0.618]
           #:border-color [border-color 'DodgerBlue] #:edge-color [edge-color 'DeepSkyBlue] #:edge-alpha [edge-alpha 0.20]
           #:head-color [head-fill #false] #:head-alpha [head-alpha #false]
           #:tail-color [tail-fill 'SeaShell] #:tail-alpha [tail-alpha #false]
           #:feet-fill-color [feet-fill #false] #:feet-alpha [feet-alpha #false]
           #:left-foot-border-color [lfcolor 'Blue] #:right-foot-border-color [rfcolor 'Lime] #:feet-rotation [foot-rotation 0.0]
           R]
    (define-values (Rdart Rcollar) (values (* R 1/phi) (* R 0.5 1/phi)))
    (define no-sheath? : Boolean (< -1.0 (* sheath-length 1.0) Rdart))
    (define-values (thickness smart-thickness-scale) (values (/ R 32.0) (if no-sheath? 1.0 2.0)))
    (define edge-stroke (desc-stroke #:color (rgb* edge-color edge-alpha) #:width (* thickness 0.5)))
    (define ihead-stroke (desc-stroke #:color edge-color #:width (* thickness 1.0)))
    (define ohead-stroke (desc-stroke #:color border-color #:width (* thickness 2.0) #:join 'round))
    (define stx-bstroke (desc-stroke #:width (* thickness smart-thickness-scale)))
    (define stx-estroke (desc-stroke #:color (rgb* edge-color edge-alpha) #:width (* thickness 0.5)))
    (define fibre-stroke (desc-stroke #:color fibre-color #:width (* thickness smart-thickness-scale) #:cap 'round))
    (define head-color : Color (rgb* (or head-fill fill-color) (or head-alpha fill-alpha)))
    (define tail-color : Color (rgb* (or tail-fill fill-color) (or tail-alpha fill-alpha)))
    (define feet-color : Color (rgb* (or feet-fill fill-color) (or feet-alpha fill-alpha)))
    
    (define protein-coat
      (geo-cc-superimpose
       (geo-icosahedron-side-projection R 'edge #:edge #false #:border ohead-stroke #:fill head-color)
       (geo-text "λ" (desc-font #:size (* R 1.4) #:family "Linux Biolinum Shadow, Bold") #:color λ-color)
       (geo-icosahedron-side-projection R 'vertex #:edge edge-stroke #:border ihead-stroke)))

    (define collar (geo-dart Rcollar pi/2 #:id 'collar #:fill tail-color #:stroke ihead-stroke #:wing-angle 4pi/5 #:radian? #true))
    (define maybe-sheath : (Option Geo)
      (and (not no-sheath?)
           (geo-arrow #:fill tail-color #:stroke ohead-stroke #:shaft-thickness -1/phi #:wing-angle pi #:radian? #true
                      Rdart sheath-length pi/2)))

    (define fibre
      (let* ([fdx (if (not maybe-sheath) (* Rcollar 0.32) Rdart)]
             [fdy (* fdx tan54º)])
        (geo-polyline #:stroke fibre-stroke
                      (list (cons 0.0 fdy) (cons fdx 0.0)
                            (cons (* fdx 3.0) 0.0) (cons (* fdx 4.0) fdy)))))
    
    (define stx-tree
      (let* ([r (* Rcollar 1/phi (if (not maybe-sheath) 0.382 1.0))]
             [fx (* (+ 1.0 cos54º) 0.5)]
             [fy (* (- 1.0 sin54º) 0.25)]
             [lft (geo-icosahedron-over-projection #:border (desc-stroke stx-bstroke #:color lfcolor) #:edge stx-estroke #:fill feet-color
                                                   #:rotation foot-rotation
                                                   r 'face)]
             [rgt (geo-icosahedron-over-projection #:border (desc-stroke stx-bstroke #:color rfcolor) #:edge stx-estroke #:fill feet-color
                                                   #:rotation foot-rotation
                                                   r 'face)])
        (if (or lfcolor rfcolor)
            (geo-pin* 1.0 1.0 (- 1.0 fx) 1.0
                      (geo-pin* 0.0 1.0 fx fy fibre (if (and lfcolor) lft (geo-ghost lft)))
                      (if (and rfcolor) rgt (geo-ghost rgt)))
            fibre)))

    (if (and maybe-sheath)
        
        (geo-vc-append #:gapsize (* Rdart -1.0) #:operator 'dest-over
                       (geo-vc-append #:gapsize (* Rcollar -1.5) #:operator 'over
                                      protein-coat
                                      (geo-cb-superimpose (geo-pin* #:operator 'clear 0.5 0.80 0.5 0.0 collar maybe-sheath)
                                                          maybe-sheath))
                       stx-tree)
        
        (geo-vc-append #:gapsize (* Rcollar -0.382) #:operator 'dest-over
                       (geo-vc-append #:gapsize (* Rcollar -1.5) #:operator 'over protein-coat collar)
                       stx-tree))))



(module+ main
  (bacteriophage-logo 2.0)
  (bacteriophage-logo 8.0)
  (bacteriophage-logo 16.0)
  (bacteriophage-logo 32.0)
  (bacteriophage-logo 64.0)
  (bacteriophage-logo 128.0)
  (bacteriophage-logo 128.0 #:sheath-length -0.8))

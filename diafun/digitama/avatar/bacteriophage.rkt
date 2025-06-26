#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)
(require racket/math)

(require geofun/vector)
(require geofun/projection)
(require geofun/digitama/geometry/constants)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bacteriophage-logo
  (lambda [#:sheath-length [sheath-length : Real+% `(,phi :)] #:λ-color [λ-color : Color 'Crimson] #:fibre-color [fibre-color : Color 'Teal]
           #:fill-color [fill-color : Color 'MintCream] #:fill-alpha [fill-alpha : Real 1/phi]
           #:border-color [border-color : Color 'DodgerBlue] #:edge-color [edge-color : Color 'DeepSkyBlue] #:edge-alpha [edge-alpha : Real 0.20]
           #:head-color [head-fill : (Option Color) #false] #:head-alpha [head-alpha : (Option Real) #false]
           #:tail-color [tail-fill : (Option Color) 'SeaShell] #:tail-alpha [tail-alpha : (Option Real) #false] #:tail.deg [tail.deg : Real +nan.0]
           #:feet-fill-color [feet-fill : (Option Real) #false] #:feet-alpha [feet-alpha : (Option Real) #false]
           #:left-foot-border-color [lfcolor : (Option Color) 'Blue] #:right-foot-border-color [rfcolor : (Option Color) 'Lime]
           #:feet-rotation [foot-rotation : Real 0.0] #:id [id : (Option Symbol) 'bacteriophage]
           #:symbol [symtext : Char #\λ]
           [R : Real]] : Geo
    (define-values (Rdart Rcollar) (values (~length (* R 1/phi)) (~length (* R 0.5 1/phi))))
    (define used-sheath-length (~length sheath-length Rdart))
    (define no-sheath? : Boolean (< used-sheath-length Rdart))
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
      (geo-cc-superimpose #:id 'protein-coat
       (geo-icosahedron-side-projection R 'edge   #:id 'coat/out #:edge #false #:border ohead-stroke #:fill head-color)
       (geo-trim (geo-text symtext (desc-font #:size (* R 1.4) #:family "Linux Biolinum Shadow, Bold") #:id 'lambda #:color λ-color))
       (geo-icosahedron-side-projection R 'vertex #:id 'coat/in #:edge edge-stroke #:border ihead-stroke)))

    (define collar (geo-dart Rcollar pi/2 #:id 'collar #:fill tail-color #:stroke ihead-stroke #:wing-angle 4pi/5))
    (define maybe-sheath : (Option Geo)
      (and (not no-sheath?)
           (geo-arrow #:id 'sheath #:fill tail-color #:stroke ohead-stroke #:shaft-thickness `(,1/phi :) #:wing-angle pi
                      Rdart used-sheath-length pi/2)))

    (define tail.rad : Flonum
      (real->double-flonum
       (degrees->radians (if (<= 10.0 tail.deg 80.0) tail.deg 54.0))))
    
    (define fibre
      (let* ([fdx (if (not maybe-sheath) (* Rcollar 0.32) Rdart)]
             [fdy (* fdx (tan tail.rad))])
        (geo-polyline #:id 'fibre #:stroke fibre-stroke
                      (list (cons 0.0 fdy) (cons fdx 0.0)
                            (cons (* fdx 3.0) 0.0) (cons (* fdx 4.0) fdy)))))

    (define stx-tree
      (let* ([fx (* (+ 1.0 (cos tail.rad)) 0.5)]
             [fy (* (- 1.0 (sin tail.rad)) 0.25)]
             [r (* Rcollar 1/phi (if (not maybe-sheath) 0.382 1.0))]
             [lft (geo-icosahedron-over-projection #:id 'lfoot #:rotation foot-rotation
                                                   #:border (desc-stroke stx-bstroke #:color lfcolor) #:edge stx-estroke #:fill feet-color
                                                   r 'face)]
             [rgt (geo-icosahedron-over-projection #:id 'rfoot #:rotation foot-rotation
                                                   #:border (desc-stroke stx-bstroke #:color rfcolor) #:edge stx-estroke #:fill feet-color
                                                   r 'face)])
        (if (or lfcolor rfcolor)
            (geo-pin* #:id 'stx-tree #:operator 'over
                      1.0 1.0 (- 1.0 fx) 1.0
                      (geo-pin* #:id 'ltree #:operator 'over
                                0.0 1.0 fx fy fibre
                                (if (and lfcolor) lft (geo-ghost lft)))
                      (if (and rfcolor) rgt (geo-ghost rgt)))
            fibre)))

    (if (and maybe-sheath)
        
        (geo-vc-append #:id id #:gapsize (* Rdart -1.0) #:operator 'dest-over
                       (geo-vc-append #:id 'head+tail #:gapsize (* Rcollar -1.5) #:operator 'over
                                      protein-coat
                                      (geo-pin* #:id 'tail #:operator 'source 0.5 0.78 0.5 0.0 collar maybe-sheath))
                       stx-tree)
        
        (geo-vc-append #:id id #:gapsize (* Rcollar -0.55) #:operator 'dest-over
                       (geo-vc-append #:id 'head+tail #:gapsize (* Rcollar -1.5) #:operator 'over protein-coat collar)
                       stx-tree))))



(module+ main
  (bacteriophage-logo 2.0)
  (bacteriophage-logo 8.0)
  (bacteriophage-logo 16.0)
  (bacteriophage-logo 32.0)
  (bacteriophage-logo 64.0)
  (bacteriophage-logo 128.0)
  (bacteriophage-logo 128.0 #:sheath-length '(80 %)))

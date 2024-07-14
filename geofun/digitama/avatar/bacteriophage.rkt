#lang typed/racket/base

(provide (all-defined-out))

(require bitmap)
(require bitmap/projection)

(require racket/math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bacteriophage-logo : (->* (Real)
                                  (#:sheath-length Real #:perspective-alpha Real
                                   #:left-foot-color (Option Color) #:right-foot-color (Option Color) #:feet-rotation Real)
                                  Bitmap)
  (lambda [R #:sheath-length [sheath-length -1.618] #:perspective-alpha [perspective-alpha 0.618]
             #:left-foot-color [lfcolor 'Blue] #:right-foot-color [rfcolor 'Lime] #:feet-rotation [foot-rotation 0.0]]
    (define-values (Rdart Rcollar) (values (* R 0.618) (* R 0.5 0.618)))
    (define no-sheath? : Boolean (< -1.0 (* sheath-length 1.0) Rdart))
    (define-values (thickness smart-thickness-scale) (values (/ R 32.0) (if no-sheath? 1.0 2.0)))
    (define edge-stroke (desc-stroke #:color 'SkyBlue #:width thickness))
    (define ihead-stroke (desc-stroke #:color 'DeepSkyBlue #:width (* thickness 2.0)))
    (define ohead-stroke (desc-stroke ihead-stroke #:color 'DodgerBlue))
    (define stx-bstroke (desc-stroke #:width (* thickness smart-thickness-scale)))
    (define stx-stroke (desc-stroke #:color (rgb* 'Snow perspective-alpha) #:width (* thickness 1.0)))
    (define fibre-stroke (desc-stroke #:color 'Teal #:width (* thickness smart-thickness-scale)))
    (define tail-color : Color 'SteelBlue)
    (define foot-color : Color (rgb* 'MintCream perspective-alpha))
    (define sin54 : Flonum (sin (degrees->radians 54.0)))
    
    (define protein-coat
      (bitmap-cc-superimpose
       (bitmap-icosahedron-side-projection R 'edge #:edge #false #:border ohead-stroke #:fill foot-color)
       (bitmap-text "λ" (desc-font #:size (* R 1.4) #:family "Linux Biolinum Shadow, Bold") #:color 'Crimson)
       (bitmap-icosahedron-side-projection R 'vertex #:edge edge-stroke #:border ihead-stroke)))

    (define collar (bitmap-dart Rcollar 90.0 #:radian? #false #:fill tail-color #:border ihead-stroke #:wing-angle 144.0))
    (define maybe-sheath : (Option Bitmap)
      (and (not no-sheath?)
           (bitmap-arrow Rdart sheath-length 90.0 #:radian? #false #:fill tail-color #:border ohead-stroke #:shaft-thickness -0.618 #:wing-angle 180.0)))

    (define fibre
      (let* ([fdx (if (not maybe-sheath) (* Rcollar 0.48) Rdart)]
             [fdy (/ fdx sin54)])
        (bitmap-polyline #:stroke fibre-stroke
                         (list (cons 0.0 fdy) (cons fdx 0.0)
                               (cons (* fdx 3.0) 0.0) (cons (* fdx 4.0) fdy)))))

    (define stx-tree
      (let*-values ([(r fx fy) (values (* Rcollar 0.618 (if (not maybe-sheath) 0.382 1.0)) 0.64 0.28)]
                    [(lft) (bitmap-icosahedron-over-projection #:border (desc-stroke stx-bstroke #:color lfcolor) #:edge stx-stroke #:fill foot-color
                                                               #:rotation foot-rotation
                                                               r 'face)]
                    [(rgt) (bitmap-icosahedron-over-projection #:border (desc-stroke stx-bstroke #:color rfcolor) #:edge stx-stroke #:fill foot-color
                                                               #:rotation foot-rotation
                                                               r 'face)])
        (if (or lfcolor rfcolor)
            (bitmap-pin* 1.0 1.0 (- 1.0 fx) 1.0
                         (bitmap-pin* 0.0 1.0 fx fy fibre (if (and lfcolor) lft (bitmap-ghost lft)))
                         (if (and rfcolor) rgt (bitmap-ghost rgt)))
            fibre)))


    (if (and maybe-sheath)
        (bitmap-vc-append #:gapsize (* Rcollar -1.618)
                          protein-coat
                          (parameterize ([default-pin-operator 'overlay])
                            (bitmap-vc-append #:gapsize (- (* Rdart -1.0) (* (stroke-width fibre-stroke) 1.0))
                                              (parameterize ([default-pin-operator 'over])
                                                (bitmap-vc-append #:gapsize (* Rcollar -0.618) collar maybe-sheath))
                                              stx-tree)))
        (parameterize ([default-pin-operator 'dest-over])
          (bitmap-vc-append #:gapsize (- (* Rcollar -0.618) (stroke-width fibre-stroke))
                            (parameterize ([default-pin-operator 'over])
                              (bitmap-vc-append #:gapsize (* Rcollar -1.618) protein-coat collar))
                            stx-tree)))))




(module+ main
  (bacteriophage-logo 2.0)
  (bacteriophage-logo 8.0)
  (bacteriophage-logo 16.0)
  (bacteriophage-logo 32.0)
  (bacteriophage-logo 64.0)
  (bacteriophage-logo 128.0)
  (bacteriophage-logo 64.0 #:sheath-length -0.8)

  (bitmap-frame (bacteriophage-logo 128.0) #:fill 'ForestGreen))
  
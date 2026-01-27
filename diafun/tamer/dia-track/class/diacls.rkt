#lang typed/racket/base

(provide (all-defined-out))

(require diafun/class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cls-colorize : Cls-Block-Theme-Adjuster
  (lambda [self anchor datum]
    (cond [(memq anchor '(Sprite Atlas Tracklet Continent Dimensionlet))
           (remake-dia-block-style self #:fill-paint 'RoyalBlue)]
          [(memq anchor '(IPlotlet IShapelet ICanvaslet ITextlet IValuelet))
           (remake-dia-block-style self #:fill-paint 'DarkKhaki)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-class-diagram! diacls.dia
  #:parameterize ([default-cls-block-theme-adjuster cls-colorize])
  #:start 'Dia:Class [#:frame 'White] #:-
  (move-up 2.0 'Dia:Track#A)
  (move-left 2.5 'Geo:Group)

  (jump-to 'Dia:Class)
  (move-left-up   2 0.75 'OnionSkin (gtmult 1 1 'composition))
  (move-down-left 2 0.75 '#:IScreen)
  (jump-to 'Dia:Class)
  (move-left-down 2 0.75 'LinkedPlaneInfo (gtmult 1 1 'composition))
  (move-down 1.75 '#:IPlaneInfo)
  (move-to '#:IScreen #false (gtmult 1 1 'aggregation))

  (jump-to 'Dia:Class)
  (move-down '#:IPlaneInfo 'Plane (gtmult 1 '0..n 'aggregation))
  (move-to '#:IPlaneInfo #false "«use»")

  (jump-to 'Plane)
  (move-down 2.5 'Matter (gtmult 1 '0..n 'composition))
  (jump-left '#:IPlaneInfo 'Continent)
  (move-left '#:IScreen #false (gtmult 1 #false))
  (move-up '#:IPlaneInfo 'Pasteboard (gtmult #false 1 'composition))
  (move-up '#:IScreen)
  (jump-to 'Continent) (move-to '#:IPlaneInfo #false (gtmult 1 1 'composition))
  (jump-to 'Continent) (move-to 'Plane #false (gtmult 1 1 'aggregation))
  (jump-to 'Continent) (move-to 'Matter)
  (move-right 2.5 '#:IMovable)

  (jump-to 'Plane)
  (move-right 2.5 'MatterInfo (gtmult 1 1 'composition))
  (move-down 1.5 '#:IMatterInfo)
  (move-to 'Plane #false (gtmult 1 1 'aggregation))
  (jump-to 'Matter)
  (move-to '#:IMatterInfo #false "«use»"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  diacls.dia)

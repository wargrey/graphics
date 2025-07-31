#lang typed/racket/base

(provide (all-defined-out))

(require diafun/class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define class-color : (Dia-Path-Node-Style-Make DiaCls-Class-Style)
  (lambda [anchor hint]
    (cond [(memq anchor '(Sprite Atlas Tracklet Continent Dimensionlet))
           (make-diacls-class-style #:fill-paint 'RoyalBlue)]
          [(memq anchor '(IPlotlet IShapelet ICanvaslet ITextlet IValuelet))
           (make-diacls-class-style #:fill-paint 'DarkKhaki)]
          [else (make-diacls-class-style)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-simple-class! cosmos.dia
  #:parameterize ([default-diacls-class-style-make class-color])
  #:start 'Cosmos [#:background 'White] #:-
  (move-up 2.0 'Universe)
  (move-left 2.5 '#:IDisplay)

  (jump-to 'Cosmos)
  (move-left-up   2 0.75 'OnionSkin (gtmult 1 1 'composition))
  (move-down-left 2 0.75 '#:IScreen)
  (jump-to 'Cosmos)
  (move-left-down 2 0.75 'LinkedPlaneInfo (gtmult 1 1 'composition))
  (move-down 1.75 '#:IPlaneInfo)
  (move-to '#:IScreen #false (gtmult 1 1 'aggregation))

  (jump-to 'Cosmos)
  (move-down '#:IPlaneInfo 'Plane (gtmult 1 '0..n 'aggregation))
  (move-to '#:IPlaneInfo #false "<<use>>")

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
  (move-to '#:IMatterInfo #false "<<use>>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  cosmos.dia)

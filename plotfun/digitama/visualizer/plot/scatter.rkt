#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

(require geofun/stroke)
(require geofun/fill)

(require geofun/digitama/base)
(require geofun/digitama/self)
(require geofun/digitama/paint)

(require geofun/digitama/path/marker)
(require geofun/digitama/path/markers)
(require geofun/digitama/path/marker/dot)
(require geofun/digitama/path/marker/self)

(require geofun/digitama/paint/self)
(require geofun/digitama/geometry/dot)
(require geofun/digitama/geometry/bleed)
(require geofun/digitama/geometry/footprint)
(require geofun/digitama/nice/pairable)

(require "line.rkt")

(require "../dot.rkt")
(require "../self.rkt")
(require "../interface.rkt")

(require "../../unsafe/dot.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct plot:scatter geo:line:visualizer
  ([marker : Geo-Path-Prints]
   [config : Geo-Marker-Config])
  #:type-name Plot:Scatter
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define points
  (lambda [#:id [id : (Option Symbol) #false]
           #:scale [scale : Pairable-Real 1.0]
           #:offset [offset : Complex 0.0+0.0i]
           #:color [color : (Option Color) #false]
           #:width [strk-width : (Option Real) #false]
           #:dash [strk-dash : (Option Stroke-Dash+Offset) #false]
           #:opacity [opacity : (Option Real) #false]
           #:fill-rule [fill-rule : (Option Fill-Rule) #false]
           #:marker [marker : Option-Geo-Marker the-bullet.mrk]
           #:clip? [clip? : Boolean #false]
           [pts : (Listof Point2D)]
           [maybe-xmin : (Option Real) #false] [maybe-xmax : (Option Real) #false]
           [maybe-ymin : (Option Real) #false] [maybe-ymax : (Option Real) #false]] : Plot-Visualizer
    (define-values (sx sy) (2d-scale-values scale))
    (define-values (points lx ty rx by) (~point2ds pts offset sx sy))
    (define-values (xrange yrange)
      (plot-range-normalize (or maybe-xmin lx) (or maybe-xmax rx)
                            (or maybe-ymin ty) (or maybe-ymax by)))
    
    (define scatter-realize : Plot-Visualizer-Realize
      (λ [idx total xmin xmax ymin ymax transform bg-color]
        (define-values (dots x y width height) (~cartesian2ds points xmin xmax ymin ymax transform))
        
        (define pen : Pen
          (plot-desc-pen #:dash strk-dash #:width strk-width #:opacity opacity
                         #:color (plot-select-pen-color color idx bg-color)
                         (default-plot-sticker-pen)))

        (define-values (shape _x _y _w _h _off cfg)
          (geo-marker-shape (geo-marker-filter marker) (pen-width pen) -pi/2 #true 'center))
        
        (create-visualizer plot:scatter
                           #:with [id (geo-draw-points pen fill-rule clip?)
                                      (geo-shape-extent width height 0.0 0.0)
                                      geo-zero-bleeds

                                      #:position (make-rectangular x y)
                                      #:color (pen-color pen)]
                           dots shape cfg)))

    (plot-visualizer scatter-realize xrange yrange
                     (plot-lines-range points)
                     (and color #true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-draw-points : (-> Maybe-Stroke-Paint (Option Fill-Rule) Boolean Geo-Surface-Draw!)
  (lambda [alt-stroke fill-rule clip?]
    (λ [self cr x0 y0 width height]
      (when (plot:scatter? self)
        (define mrkcfg (plot:scatter-config self))
        (define paint (geo-select-stroke-paint alt-stroke))
        (define-values (pen brush) (geo-marker-resolve-paints paint mrkcfg fill-rule))
        
        (dc_scatter cr x0 y0 width height
                    (geo:visualizer-position self) (geo:line:visualizer-dots self) (plot:scatter-marker self)
                    pen brush clip?)))))

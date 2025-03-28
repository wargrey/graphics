#lang typed/racket/base

(provide (all-defined-out))

(require geofun/paint)
(require geofun/stroke)

(require geofun/digitama/source)
(require geofun/digitama/convert)
(require geofun/digitama/geometry/dot)

(require geofun/digitama/unsafe/dc/path)

(require "self.rkt")
(require "../interface.rkt")
(require "../../sample.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct plot:function geo
  ([dots : (Listof Float-Complex)]
   [samples : Positive-Index]
   [tx : Flonum]
   [ty : Flonum])
  #:type-name Plot:Function
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define function
  (lambda [#:id [id : (Option Symbol) #false] #:label [label : Any #false]
           #:samples [samples : Positive-Index (default-plot-renderer-samples)]
           #:stroke [stroke : Stroke-Paint (default-plot-function-stroke)]
           [f : (-> Flonum (Option Flonum))]
           [maybe-xmin : (Option Real) #false] [maybe-xmax : (Option Real) #false]
           [maybe-ymin : (Option Real) #false] [maybe-ymax : (Option Real) #false]] : Plot:Renderer
    (define-values (xrange yrange) (plot-range-normalize maybe-xmin maybe-xmax maybe-ymin maybe-ymax))
    (define pen : Stroke (stroke-paint->source stroke (default-plot-function-stroke)))
    (define draw! : Geo-Surface-Draw! (plot-draw-function pen))

    (define plot-function-realize : Plot-Renderer-Realize
      (lambda [xmin xmax ymin ymax sx sy transform]
        (define xs : (Listof Real) (geo-linear-samples xmin xmax samples))
        (define-values (dots lx ty rx by) (~cartesian2ds f xs ymin ymax sx sy transform))
        (define-values (xoff yoff width height x-stroke? y-stroke?) (point2d->window +nan.0+nan.0i lx ty rx by))
        
        (create-geometry-object plot:function
                                #:with [id draw!
                                           (geo-shape-extent width height 0.0 0.0)
                                           (geo-shape-outline stroke x-stroke? y-stroke?)]
                                dots samples xoff yoff)))

    (plot:renderer plot-function-realize
                   (or label (object-name f))
                   xrange yrange (stroke-color pen))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-draw-function : (-> Stroke Geo-Surface-Draw!)
  (lambda [pen]
    (λ [self cr x0 y0 width height]
      (when (plot:function? self)
        (define-values (tx ty) (values (plot:function-tx self) (plot:function-ty self)))
        (dc_polyline cr (+ x0 tx) (+ y0 ty) width height
                     (plot:function-dots self) pen #false)))))

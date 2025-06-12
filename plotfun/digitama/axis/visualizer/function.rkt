#lang typed/racket/base

(provide (all-defined-out))

(require geofun/paint)

(require geofun/digitama/base)
(require geofun/digitama/paint)
(require geofun/digitama/markup)
(require geofun/digitama/convert)

(require geofun/digitama/paint/self)
(require geofun/digitama/unsafe/dc/path)

(require "self.rkt")
(require "../interface.rkt")
(require "../../sample.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct plot:function geo:visualizer
  ([dots : (Listof Float-Complex)]
   [samples : Positive-Index])
  #:type-name Plot:Function
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define function
  (lambda [#:id [id : (Option Symbol) #false]
           #:label [label : (Option DC-Markup-Text) #false]
           #:samples [samples : Positive-Index (default-plot-visualizer-samples)]
           #:color [pen-color : (Option Color) #false]
           #:width [pen-width : (Option Real) #false]
           #:dash [pen-dash : (Option Stroke-Dash+Offset) #false]
           #:fast-range [fast-range : (Option Plot-Visualizer-Data-Range) #false]
           [f : (-> Real (Option Number))]
           [maybe-xmin : (Option Real) #false] [maybe-xmax : (Option Real) #false]
           [maybe-ymin : (Option Real) #false] [maybe-ymax : (Option Real) #false]] : Plot-Visualizer
    (define-values (xrange yrange) (plot-range-normalize maybe-xmin maybe-xmax maybe-ymin maybe-ymax))
    
    (define function-realize : Plot-Visualizer-Realize
      (lambda [idx xtick-rng ymin ymax transform bg-color]
        (define xs : (Listof Real) (geo-linear-samples (car xtick-rng) (cdr xtick-rng) samples))
        (define-values (dots x y width height)
          (with-handlers ([exn:fail? (λ [_] (~cartesian2ds (plot-safe-function f) xs ymin ymax transform))])
            (~cartesian2ds f xs ymin ymax transform)))

        (define pen : Stroke
          (plot-desc-pen #:width pen-width #:dash pen-dash
                         #:color (plot-select-pen-color pen-color idx bg-color)))
        
        (create-geometry-object plot:function
                                #:with [id (geo-draw-function pen)
                                           (geo-shape-extent width height 0.0 0.0)
                                           (geo-shape-outline pen #true #true)]
                                (make-rectangular x y) #false
                                dots samples)))

    (plot-visualizer function-realize xrange yrange
                     (or fast-range (plot-function-range f samples)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-function-range : (-> (-> Real (Option Number)) Positive-Index Plot-Visualizer-Data-Range)
  (let ([plot-function-range-db : (Weak-HashTable Any (Pairof Real Real)) (make-weak-hash)])
    (lambda [f samples]
      (λ [xmin xmax]
        (hash-ref! plot-function-range-db (list f xmin xmax samples)
                   (λ [] (let ([xs (geo-linear-samples xmin xmax samples)])
                           (define maybe-range
                             (with-handlers ([exn:fail? (λ _ #false)])
                               (~y-bounds f xs)))
                           (or maybe-range
                               (~y-bounds (plot-safe-function f) xs)))))))))
  
(define #:forall (R) plot-safe-function : (-> (-> R (Option Number)) (-> R (Option Number)))
  (lambda [f]
    ((inst procedure-rename (-> R (Option Number)))
     (λ [[x : R]]
       (with-handlers ([exn:fail? (λ _ #false)])
         (f x)))
     (assert (object-name f) symbol?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-draw-function : (-> Maybe-Stroke-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke]
    (λ [self cr x0 y0 width height]
      (when (plot:function? self)
        (define pos (geo:visualizer-position self))
        (dc_polyline cr (- x0 (real-part pos)) (- y0 (imag-part pos)) width height
                     (plot:function-dots self)
                     (geo-select-stroke-paint* alt-stroke) #false)))))

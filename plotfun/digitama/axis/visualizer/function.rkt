#lang typed/racket/base

(provide (all-defined-out))

(require geofun/paint)
(require geofun/color)

(require geofun/digitama/base)
(require geofun/digitama/markup)
(require geofun/digitama/convert)
(require geofun/digitama/dc/polygon)

(require geofun/digitama/paint/self)
(require geofun/digitama/paint/source)

(require "self.rkt")
(require "../interface.rkt")
(require "../../sample.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct plot:function geo:polyline
  ([samples : Positive-Index])
  #:type-name Plot:Function
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define function
  (lambda [#:id [id : (Option Symbol) #false] #:label [label : (Option DC-Markup-Text) #false]
           #:samples [samples : Positive-Index (default-plot-visualizer-samples)]
           #:color [pen-color : (Option Color) #false]
           #:width [pen-width : (Option Real) #false]
           #:dash [pen-dash : (Option Stroke-Dash+Offset) #false]
           #:fast-range [fast-range : (Option Plot-Visualizer-Data-Range) #false]
           [f : (-> Real (Option Number))]
           [maybe-xmin : (Option Real) #false] [maybe-xmax : (Option Real) #false]
           [maybe-ymin : (Option Real) #false] [maybe-ymax : (Option Real) #false]] : Plot:Visualizer
    (define-values (xrange yrange) (plot-range-normalize maybe-xmin maybe-xmax maybe-ymin maybe-ymax))
    
    (define plot-function-realize : Plot-Visualizer-Realize
      (lambda [idx xtick-rng ytick-rng transform L-bg]
        (define xs : (Listof Real) (geo-linear-samples (car xtick-rng) (cdr xtick-rng) samples))
        (define-values (dots x y width height)
          (let-values ([(dots x y width height)
                        (with-handlers ([exn:fail? (λ _ (values #false 0.0 0.0 0.0 0.0))])
                          (~cartesian2ds f xs (car ytick-rng) (cdr ytick-rng) transform))])
            (if (not dots)
                (~cartesian2ds (plot-safe-function f) xs (car ytick-rng) (cdr ytick-rng) transform)
                (values dots x y width height))))

        (define pen : Stroke
          (plot-desc-pen #:width pen-width #:dash pen-dash
                         #:color (or pen-color
                                     (let ([cs ((default-plot-palette) idx L-bg)])
                                       (car cs)))))
        
        (values (create-geometry-object plot:function
                                        #:with [id (geo-draw-polyline pen)
                                                   (geo-shape-extent width height 0.0 0.0)
                                                   (geo-shape-outline pen #true #true)]
                                        dots (- x) (- y) #false samples)
                (make-rectangular x y)
                #false)))

    (define plot-function-range : Plot-Visualizer-Data-Range
      (lambda [xmin xmax]
        (hash-ref! plot-function-range-db (list f xmin xmax samples)
                   (λ [] (let ([xs (geo-linear-samples xmin xmax samples)])
                           (define maybe-range
                             (with-handlers ([exn:fail? (λ _ #false)])
                               (~y-bounds f xs)))
                           (or maybe-range
                               (~y-bounds (plot-safe-function f) xs)))))))

    (plot:visualizer plot-function-realize xrange yrange
                     (or fast-range plot-function-range))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-function-range-db : (Weak-HashTable Any (Pairof Real Real)) (make-weak-hash))

(define #:forall (R) plot-safe-function : (-> (-> R (Option Number)) (-> R (Option Number)))
  (lambda [f]
    ((inst procedure-rename (-> R (Option Number)))
     (λ [[x : R]]
       (with-handlers ([exn:fail? (λ _ #false)])
         (f x)))
     (assert (object-name f) symbol?))))

#lang typed/racket/base

(provide (all-defined-out))

(require geofun/paint)

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
  (lambda [#:id [id : (Option Symbol) #false] #:label [label : Any #false]
           #:samples [samples : Positive-Index (default-plot-visualizer-samples)]
           #:stroke [stroke : Stroke-Paint (default-plot-function-stroke)]
           #:fast-range [fast-range : (Option Plot-Visualizer-Data-Range) #false]
           #:safe? [safe? : Boolean #true]
           [f : (-> Real (Option Number))]
           [maybe-xmin : (Option Real) #false] [maybe-xmax : (Option Real) #false]
           [maybe-ymin : (Option Real) #false] [maybe-ymax : (Option Real) #false]] : Plot:Visualizer
    (define-values (xrange yrange) (plot-range-normalize maybe-xmin maybe-xmax maybe-ymin maybe-ymax))
    (define pen : Stroke (stroke-paint->source stroke (default-plot-function-stroke)))
    (define safe-f : (-> Real (Option Number)) (if (not safe?) (plot-safe-function f) f))

    (define plot-function-realize : Plot-Visualizer-Realize
      (lambda [xtick-rng ytick-rng transform]
        (define xs : (Listof Real) (geo-linear-samples (car xtick-rng) (cdr xtick-rng) samples))
        (define-values (dots x y width height) (~cartesian2ds safe-f xs (car ytick-rng) (cdr ytick-rng) transform))
        
        (values (create-geometry-object plot:function
                                        #:with [id (geo-draw-polyline pen)
                                                   (geo-shape-extent width height 0.0 0.0)
                                                   (geo-shape-outline pen #true #true)]
                                        dots (- x) (- y) #false samples)
                (make-rectangular x y))))

    (define plot-function-range : Plot-Visualizer-Data-Range
      (lambda [xmin xmax]
        (hash-ref! plot-function-range-db (list safe-f xmin xmax samples)
                   (λ [] (let ([xs (geo-linear-samples xmin xmax samples)])
                           (define-values (ymin ymax) (~y-bounds safe-f xs))
                           (cons ymin ymax))))))

    (plot:visualizer plot-function-realize
                     (or label (object-name safe-f))
                     xrange yrange (stroke-color pen)
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

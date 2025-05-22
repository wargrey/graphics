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
   [samples : Positive-Index])
  #:type-name Plot:Function
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define function
  (lambda [#:id [id : (Option Symbol) #false] #:label [label : Any #false]
           #:samples [samples : Positive-Index (default-plot-renderer-samples)]
           #:stroke [stroke : Stroke-Paint (default-plot-function-stroke)]
           #:fast-range [fast-range : (Option Plot-Renderer-Data-Range) #false]
           #:safe? [safe? : Boolean #true]
           [f : (-> Real (Option Number))]
           [maybe-xmin : (Option Real) #false] [maybe-xmax : (Option Real) #false]
           [maybe-ymin : (Option Real) #false] [maybe-ymax : (Option Real) #false]] : Plot:Renderer
    (define-values (xrange yrange) (plot-range-normalize maybe-xmin maybe-xmax maybe-ymin maybe-ymax))
    (define pen : Stroke (stroke-paint->source stroke (default-plot-function-stroke)))
    (define safe-f : (-> Real (Option Number)) (if (not safe?) (plot-safe-function f) f))
    (define draw! : Geo-Surface-Draw! (plot-draw-function pen))

    (define plot-function-realize : Plot-Renderer-Realize
      (lambda [xtick-rng ytick-rng transform]
        (define xs : (Listof Real) (geo-linear-samples (car xtick-rng) (cdr xtick-rng) samples))
        (define-values (dots position width height) (~cartesian2ds safe-f xs (car ytick-rng) (cdr ytick-rng) transform))
        
        (values (create-geometry-object plot:function
                                        #:with [id draw!
                                                   (geo-shape-extent width height 0.0 0.0)
                                                   (geo-shape-outline stroke #true #true)]
                                        dots samples)
                position)))

    (define plot-function-range : Plot-Renderer-Data-Range
      (lambda [xmin xmax]
        (hash-ref! plot-function-range-db (list safe-f xmin xmax samples)
                   (λ [] (let ([xs (geo-linear-samples xmin xmax samples)])
                           (define-values (ymin ymax) (~y-bounds safe-f xs))
                           (cons ymin ymax))))))

    (plot:renderer plot-function-realize
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

(define plot-draw-function : (-> Stroke Geo-Surface-Draw!)
  (lambda [pen]
    (λ [self cr x0 y0 width height]
      (when (plot:function? self)
        (dc_polyline cr x0 y0 width height
                     (plot:function-dots self) pen #false)))))

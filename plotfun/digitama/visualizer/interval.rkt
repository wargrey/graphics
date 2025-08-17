#lang typed/racket/base

(provide (all-defined-out))

(require racket/format)

(require geofun/paint)
(require geofun/color)

(require geofun/digitama/base)
(require geofun/digitama/paint)
(require geofun/digitama/markup)
(require geofun/digitama/convert)

(require geofun/digitama/paint/self)

(require "dot.rkt")
(require "self.rkt")
(require "guard.rkt")
(require "interface.rkt")

(require "../sample.rkt")
(require "../calculus.rkt")

(require "../marker/self.rkt")
(require "../unsafe/line.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct plot:function geo:line:visualizer ()
  #:type-name Plot:Function
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define function
  (lambda [#:id [id : (Option Symbol) #false]
           #:label [label : (U False DC-Markup-Text Plot:Mark 'name) 'name]
           #:label-position [at-frac : Real (default-plot-visualizer-label-position)]
           #:label-position-range [rng-frac : (Pairof Real Real) (default-plot-visualizer-label-position-range)]
           #:label-placement [placement : Plot-Visualizer-Label-Placement (default-plot-visualizer-label-placement)]
           #:density [density : Byte (default-plot-visualizer-sample-density)]
           #:color [pen-color : (Option Color) #false]
           #:width [pen-width : (Option Real) #false]
           #:dash [pen-dash : (Option Stroke-Dash+Offset) #false]
           #:opacity [opacity : Real 1.0]
           #:fast-range [fast-range : (Option Plot-Visualizer-Data-Range) #false]
           #:safe-df/dx [alt-df/dx : (Option (-> Real Real)) #false]
           #:safe-dff/dxx [alt-ddf/dxx : (Option (-> Real Real)) #false]
           [f : (-> Real (Option Number))]
           [maybe-xmin : (Option Real) #false] [maybe-xmax : (Option Real) #false]
           [maybe-ymin : (Option Real) #false] [maybe-ymax : (Option Real) #false]] : Plot-Visualizer
    (define-values (xrange yrange) (plot-range-normalize maybe-xmin maybe-xmax maybe-ymin maybe-ymax))
    (define safe-f : (-> Real Real) (safe-real-function f))
    
    (define function-realize : Plot-Visualizer-Realize
      (λ [idx total xmin xmax ymin ymax transform bg-color]
        (define samples : Positive-Index (geo-sample-count xmin xmax transform density))
        (define xs : (Listof Real) (geo-linear-samples xmin xmax samples))
        (define-values (dots x y width height)
          (parameterize ([plot-sampling? #true])
            (with-handlers ([exn:fail? (λ [_] (~cartesian2ds safe-f xs ymin ymax transform))])
              (~cartesian2ds f xs ymin ymax transform))))

        (define dynamic-angle : (-> Real (Option Real))
          (plot-function-pin-angle (or   alt-df/dx (λ [[x : Real]]   (df/dx safe-f x)))
                                   (or alt-ddf/dxx (λ [[x : Real]] (ddf/dxx safe-f x)))
                                   placement))

        (define pen : Stroke
          (plot-desc-pen #:width pen-width #:dash pen-dash
                         #:color (rgb* (plot-select-pen-color pen-color idx bg-color)
                                       opacity)))
        
        (create-geometry-object plot:function
                                #:with [id (geo-draw-function pen)
                                           (geo-shape-extent width height 0.0 0.0)
                                           (geo-shape-outline pen #true #true)]
                                #false
                                (make-rectangular x y) (stroke-color pen)
                                (plot-function-mark-guard safe-f label (+ idx 1) total xmin xmax ymin ymax at-frac rng-frac)
                                #false dynamic-angle dynamic-angle
                                dots)))

    (plot-visualizer function-realize xrange yrange
                     (or fast-range (plot-function-range safe-f 500))
                     (and pen-color #true))))

(define f:linear
  (lambda [#:id [id : (Option Symbol) #false]
           #:label [label : (U False DC-Markup-Text Plot:Mark 'name) 'name]
           #:label-position [at-frac : Real (default-plot-visualizer-label-position)]
           #:label-position-range [rng-frac : (Pairof Real Real) (default-plot-visualizer-label-position-range)]
           #:label-placement [placement : Plot-Visualizer-Label-Placement (default-plot-visualizer-label-placement)]
           #:color [pen-color : (Option Color) #false]
           #:width [pen-width : (Option Real) #false]
           #:dash [pen-dash : (Option Stroke-Dash+Offset) #false]
           #:%.lf [%.lf : Natural 2]
           [k : Real] [b : Real]
           [maybe-xmin : (Option Real) #false] [maybe-xmax : (Option Real) #false]
           [maybe-ymin : (Option Real) #false] [maybe-ymax : (Option Real) #false]] : Plot-Visualizer
    (define-values (xrange yrange) (plot-range-normalize maybe-xmin maybe-xmax maybe-ymin maybe-ymax))
    (define f : (-> Real Real) (procedure-rename (λ [[x : Real]] (+ (* k x) b)) (plot-linear-name k b %.lf)))
    (define f⁻¹ : (-> Real Real) (λ [[y : Real]] (/ (- y b) k)))

    (define linear-range : Plot-Visualizer-Data-Range
      (lambda [xmin xmax]
        (define-values (y1 y2) (values (f xmin) (f xmax)))

        (if (< y1 y2)
            (cons y1 y2)
            (cons y2 y1))))

    (define linear-bound-guard : (-> Real Real Real (Values Real Real))
      (lambda [x ymin ymax]
        (define y (f x))
        (cond [(< y ymin) (values (f⁻¹ ymin) ymin)]
              [(> y ymax) (values (f⁻¹ ymax) ymax)]
              [else (values x y)])))

    (define linear-realize : Plot-Visualizer-Realize
      (λ [idx total xmin xmax ymin ymax transform bg-color]
        (define-values (x1 y1) (linear-bound-guard xmin ymin ymax))
        (define-values (x2 y2) (linear-bound-guard xmax ymin ymax))
        (define lft (transform (real->double-flonum x1) (real->double-flonum y1)))
        (define rgt (transform (real->double-flonum x2) (real->double-flonum y2)))
        (define width  (abs (real-part (- rgt lft))))
        (define height (abs (imag-part (- rgt lft))))
        (define dynamic-angle : (-> Real (Option Real)) (plot-function-pin-angle k 0 placement))

        (define pen : Stroke
          (plot-desc-pen #:width pen-width #:dash pen-dash
                         #:color (plot-select-pen-color pen-color idx bg-color)))
        
        (create-geometry-object plot:function
                                #:with [id (geo-draw-function pen)
                                           (geo-shape-extent width height 0.0 0.0)
                                           (geo-shape-outline pen #true #true)]
                                #false
                                (make-rectangular (min (real-part lft) (real-part rgt))
                                                  (min (imag-part lft) (imag-part rgt)))
                                (stroke-color pen)
                                (plot-function-mark-guard f label (+ idx 1) total x1 x2 y1 y2 at-frac rng-frac)
                                #false dynamic-angle dynamic-angle
                                (list lft rgt))))
    
    (plot-visualizer linear-realize xrange yrange
                     linear-range (and pen-color #true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-function-range : (-> (-> Real (Option Number)) Positive-Index Plot-Visualizer-Data-Range)
  (let ([range-db : (Weak-HashTable Any (Pairof Real Real)) (make-weak-hash)])
    (lambda [f samples]
      (λ [xmin xmax]
        (hash-ref! range-db (list f xmin xmax)
                   (λ [] (parameterize ([plot-sampling? #false])
                           (define xs (geo-linear-samples xmin xmax samples))
                           (define maybe-range
                             (with-handlers ([exn:fail? (λ _ #false)])
                               (~y-bounds f xs)))
                           (or maybe-range
                               (~y-bounds (safe-real-function f) xs)))))))))

(define plot-linear-name : (-> Real Real Natural Symbol)
  (lambda [k b %.lf]
    (let ([K (~r k #:precision %.lf)]
          [B (~r (abs b) #:precision %.lf)])
      (string->symbol
       (cond [(string=? K "0") (format "y = ~a" B)]
             [(string=? B "0") (format "y = ~ax" (case K [("1") '||] [("-1") '-] [else K]))]
             [else (format "y = ~ax ~a ~a"
                     (case K [("1") '||] [("-1") '-] [else K])
                     (if (< b 0.0) '- '+) B)])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-draw-function : (-> Maybe-Stroke-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke]
    (λ [self cr x0 y0 width height]
      (when (plot:function? self)
        (define pos (geo:visualizer-position self))
        (dc_line cr (- x0 (real-part pos)) (- y0 (imag-part pos)) width height
                 (geo:line:visualizer-dots self)
                 (geo-select-stroke-paint* alt-stroke))))))

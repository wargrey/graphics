#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)
(require racket/case)

(require geofun/paint)

(require geofun/digitama/base)
(require geofun/digitama/paint)
(require geofun/digitama/markup)
(require geofun/digitama/convert)

(require geofun/digitama/paint/self)
(require geofun/digitama/unsafe/dc/path)

(require "self.rkt")
(require "interface.rkt")

(require "../sample.rkt")
(require "../calculus.rkt")

(require "../marker/self.rkt")
(require "../marker/guard.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct plot:function geo:visualizer
  ([dots : (Listof Float-Complex)])
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
           #:fast-range [fast-range : (Option Plot-Visualizer-Data-Range) #false]
           #:safe-slope [alt-slope : (Option (-> Real Real)) #false]
           [f : (-> Real (Option Number))]
           [maybe-xmin : (Option Real) #false] [maybe-xmax : (Option Real) #false]
           [maybe-ymin : (Option Real) #false] [maybe-ymax : (Option Real) #false]] : Plot-Visualizer
    (define-values (xrange yrange) (plot-range-normalize maybe-xmin maybe-xmax maybe-ymin maybe-ymax))
    (define safe-f : (-> Real Real) (safe-real-function f))
    
    (define function-realize : Plot-Visualizer-Realize
      (lambda [idx total xmin xmax ymin ymax transform bg-color]
        (define samples (geo-sample-count xmin xmax transform density))
        (define xs : (Listof Real) (geo-linear-samples xmin xmax samples))
        (define-values (dots x y width height)
          (parameterize ([plot-sampling? #true])
            (with-handlers ([exn:fail? (λ [_] (~cartesian2ds safe-f xs ymin ymax transform))])
              (~cartesian2ds f xs ymin ymax transform))))
        (define dynamic-angle (plot-function-pin-angle safe-f alt-slope placement))

        (define pen : Stroke
          (plot-desc-pen #:width pen-width #:dash pen-dash
                         #:color (plot-select-pen-color pen-color idx bg-color)))
        
        (create-geometry-object plot:function
                                #:with [id (geo-draw-function pen)
                                           (geo-shape-extent width height 0.0 0.0)
                                           (geo-shape-outline pen #true #true)]
                                (make-rectangular x y) (stroke-color pen)
                                (plot-function-mark-guard safe-f label (+ idx 1) total xmin xmax ymin ymax at-frac rng-frac)
                                #false dynamic-angle dynamic-angle
                                dots)))

    (plot-visualizer function-realize xrange yrange
                     (or fast-range (plot-function-range safe-f 500))
                     (and pen-color #true))))

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

(define plot-function-pin-angle : (-> (-> Real Real) (Option (-> Real Real)) Plot-Visualizer-Label-Placement (-> Real (Option Real)))
  (let ([angle-db : (Weak-HashTable Any (Option Real)) (make-weak-hash)])
    (lambda [f alt-slope placement]
      (if (not alt-slope)
          (λ [[x : Real]]
            (hash-ref! angle-db (list f x placement)
                       (λ [] (let* ([k (df/dx f x)]
                                    [c (or (ddf/dxx f x) 0)]
                                    [theta (and k (atan k))])
                               (and theta (case/eq placement
                                                   [(auto) (- (* pi (if (> c 0) 0.5 1.5)) theta)]
                                                   [(flip) (- (* pi (if (> c 0) 1.5 0.5)) theta)]
                                                   [(left) (- (* pi 1.5) theta)]
                                                   [else   (- (* pi 0.5) theta)]))))))
          (plot-function-pin-angle alt-slope #false placement)))))
  
(define plot-function-mark-guard : (-> (-> Real Real) (U False DC-Markup-Text Plot:Mark 'name)
                                       Positive-Fixnum Positive-Index Real Real Real Real
                                       Real (Pairof Real Real)
                                       (Option Plot:Mark))
  (lambda [fx label idx total xmin xmax ymin ymax at-frac frac-rng]
    (cond [(plot:mark? label)
           (let* ([origin (plot:mark-point label)]
                  [dot (plot-mark-point-guard origin fx idx total xmin xmax ymin ymax at-frac frac-rng)])
             (and dot (cond [(and (= origin dot)) label]
                            [else (remake-plot:mark label #:point dot)])))]
          [(or label)
           (let ([dot (plot-mark-point-guard +nan.0 fx idx total xmin xmax ymin ymax at-frac frac-rng)])
             (and dot (let ([func-name (if (eq? label 'name) (format "~a" (object-name fx)) label)])
                        (plot-label func-name #:at dot))))]
          [else #false])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-draw-function : (-> Maybe-Stroke-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke]
    (λ [self cr x0 y0 width height]
      (when (plot:function? self)
        (define pos (geo:visualizer-position self))
        (dc_polyline cr (- x0 (real-part pos)) (- y0 (imag-part pos)) width height
                     (plot:function-dots self)
                     (geo-select-stroke-paint* alt-stroke) #false)))))

#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

(require geofun/digitama/base)
(require geofun/digitama/self)
(require geofun/digitama/paint)
(require geofun/digitama/richtext/self)

(require geofun/digitama/paint/self)
(require geofun/digitama/geometry/dot)
(require geofun/digitama/nice/pairable)

(require "polyline.rkt")

(require "../dot.rkt")
(require "../self.rkt")
(require "../guard.rkt")
(require "../interface.rkt")

(require "../../marker/self.rkt")
(require "../../marker/style.rkt")

(require "../../unsafe/dot.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define polar
  (lambda [#:id [id : (Option Symbol) #false]
           #:label [label : (U False Geo-Rich-Text Plot:Mark 'name) 'name]
           #:label-position [at-frac : Real (default-plot-visualizer-label-position)]
           #:label-position-range [rng-frac : (Pairof Real Real) (default-plot-visualizer-label-position-range)]
           #:label-placement [placement : Plot-Visualizer-Label-Placement (default-plot-visualizer-label-placement)]
           #:offset [offset : Complex 0.0+0.0i]
           #:color [strk-color : (Option Color) #false]
           #:width [strk-width : (Option Real) #false]
           #:opacity [opacity : (Option Real) #false]
           #:dash [strk-dash : (Option Stroke-Dash+Offset) #false]
           #:close? [close? : Boolean #false]
           [r : (-> Real (Option Number))]
           [θmin : Real 0] [θmax : Real 2pi]] : Plot-Visualizer
    (define-values (points lx ty rx by) (polar-points r θmin θmax 500))
    (define df/dx : (-> Real (Option Real)) (plot-lines-df/dx points))
    (define safe-f : (-> Real Real) (plot-lines->function points close? (object-name r)))
    (define-values (xrange yrange) (plot-range-normalize lx rx -inf.0 +inf.0))

    (define polar-realize : Plot-Visualizer-Realize
      (λ [idx total xmin xmax ymin ymax transform bg-color]
        (define-values (dots x y width height) (~cartesian2ds points xmin xmax ymin ymax transform))
        (define dynamic-angle : (-> Real (Option Real))
          (plot-function-pin-angle df/dx 0 placement))

        (define pen : Pen
          (plot-desc-pen #:width strk-width #:dash strk-dash #:opacity opacity
                         #:color (plot-select-pen-color strk-color idx bg-color)
                         (default-plot-function-pen)))

        (create-visualizer plot:lines
                           #:with [id (geo-draw-lines pen)
                                      (geo-shape-extent width height 0.0 0.0)
                                      (geo-shape-bleed pen #true #true)

                                      #:position (make-rectangular x y)
                                      #:color (pen-color pen)
                                      #:label (plot-function-mark-guard safe-f label (+ idx 1) total xmin xmax ymin ymax at-frac rng-frac)
                                      #:pin-angle dynamic-angle
                                      #:gap-angle dynamic-angle]
                           dots close?)))

    (plot-visualizer polar-realize xrange yrange
                     (plot-lines-range points)
                     (and strk-color #true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define polar-points : (-> (-> Real (Option Number)) Real Real Positive-Index (Values (Listof Float-Complex) Flonum Flonum Flonum Flonum))
  (let ([pts-db : (Weak-HashTable Any (Immutable-Vector (Listof Float-Complex) Flonum Flonum Flonum Flonum)) (make-weak-hash)])
    (lambda [rθ θmin θmax density]
      (define &pts : (Immutable-Vector (Listof Float-Complex) Flonum Flonum Flonum Flonum)
        (hash-ref! pts-db (list rθ θmin θmax density)
                   (λ [] (parameterize ([plot-sampling? #false])
                           (define-values (pts lx ty rx by) (~polar2ds rθ θmin θmax density))
                           (vector-immutable pts lx ty rx by)))))

      (values (vector-ref &pts 0)
              (vector-ref &pts 1) (vector-ref &pts 2)
              (vector-ref &pts 3) (vector-ref &pts 4)))))

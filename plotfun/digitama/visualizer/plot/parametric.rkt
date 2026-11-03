#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

(require geofun/digitama/base)
(require geofun/digitama/self)
(require geofun/digitama/richtext/self)
(require geofun/digitama/paint/self)

(require "polyline.rkt")

(require "../dot.rkt")
(require "../self.rkt")
(require "../curve.rkt")
(require "../guard.rkt")
(require "../interface.rkt")

(require "../../calculus.rkt")
(require "../../marker/self.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define parametric
  (lambda [#:id [id : (Option Symbol) #false]
           #:label [label : (U False Geo-Rich-Text Plot:Mark 'name) 'name]
           #:label-position [at-frac : Real (default-plot-visualizer-label-position)]
           #:label-position-range [rng-frac : (Pairof Real Real) (default-plot-visualizer-label-position-range)]
           #:label-placement [placement : Plot-Visualizer-Label-Placement (default-plot-visualizer-label-placement)]
           #:color [strk-color : (Option Color) #false]
           #:width [strk-width : (Option Real) #false]
           #:opacity [opacity : (Option Real) #false]
           #:dash [strk-dash : (Option Stroke-Dash+Offset) #false]
           #:safe-dx/dt [alt-dx/dt : (Option (-> Real (Option Real))) #false]
           #:safe-dy/dt [alt-dy/dt : (Option (-> Real (Option Real))) #false]
           #:safe-ddx/dtt [alt-ddx/dtt : (Option (-> Real (Option Real))) #false]
           #:safe-ddy/dtt [alt-ddy/dtt : (Option (-> Real (Option Real))) #false]
           [xt : (-> Real (Option Number))]
           [yt : (-> Real (Option Number))]
           [tmin : Real 0] [tmax : Real 1]] : Plot-Visualizer
    (define safe-x (safe-real-function xt))
    (define safe-y (safe-real-function yt))
    (define safe-f (parametric->cartesian-function safe-x safe-y))
    (define-values (points lx ty rx by) (parametric-points safe-x safe-y tmin tmax 500))
    (define-values (xrange yrange) (plot-range-normalize lx rx #false #false))

    (define polar-realize : Plot-Visualizer-Realize
      (λ [idx total xmin xmax ymin ymax transform bg-color]
        (define-values (dots x y width height) (~cartesian2ds points xmin xmax ymin ymax transform))

        (define dynamic-angle : (-> Real (Option Real))
          (plot-function-pin-angle (parametric-d/dt safe-x safe-y alt-dx/dt alt-dy/dt)
                                   (parametric-dd/dtt safe-x safe-y alt-dx/dt alt-ddx/dtt alt-dy/dt alt-ddy/dtt)
                                   placement))

        (define pen : Pen
          (plot-desc-pen #:width strk-width #:dash strk-dash #:opacity opacity
                         #:color (plot-select-pen-color strk-color idx bg-color)
                         (default-plot-function-pen)))

        (create-visualizer plot:curve
                           #:with [id (geo-draw-curve pen #false)
                                      (geo-shape-extent width height 0.0 0.0)
                                      (geo-shape-bleed pen #true #true)

                                      #:position (make-rectangular x y)
                                      #:color (pen-color pen)
                                      #:label (plot-function-mark-guard safe-f label [idx total tmin tmax xmin xmax ymin ymax at-frac rng-frac])
                                      #:pin-angle dynamic-angle
                                      #:gap-angle dynamic-angle]
                           dots)))

    (plot-visualizer polar-realize xrange yrange
                     (plot-lines-range points)
                     (and strk-color #true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define parametric-points : (-> (-> Real Real) (-> Real Real) Real Real Positive-Index (Values (Listof Float-Complex) Flonum Flonum Flonum Flonum))
  (let ([pts-db : (Weak-HashTable Any (Immutable-Vector (Listof Float-Complex) Flonum Flonum Flonum Flonum)) (make-weak-hash)])
    (lambda [xt yt tmin tmax density]
      (define &pts : (Immutable-Vector (Listof Float-Complex) Flonum Flonum Flonum Flonum)
        (hash-ref! pts-db (list xt yt tmin tmax density)
                   (λ [] (parameterize ([plot-sampling? #false])
                           (define-values (pts lx ty rx by) (~parametric2ds xt yt tmin tmax density))
                           (vector-immutable pts lx ty rx by)))))

      (values (vector-ref &pts 0)
              (vector-ref &pts 1) (vector-ref &pts 2)
              (vector-ref &pts 3) (vector-ref &pts 4)))))

(define parametric->cartesian-function : (-> (-> Real Real) (-> Real Real) (-> Real Number))
  (lambda [xt yt]
    (procedure-rename
     (λ [[t : Real]] (make-rectangular (xt t) (yt t)))
     (string->symbol (format "{~a, ~a}" (object-name xt) (object-name yt))))))

(define parametric-d/dt : (-> (-> Real Real) (-> Real Real) (Option (-> Real (Option Real))) (Option (-> Real (Option Real))) (-> Real (Option Real)))
  (lambda [xt yt alt-dx/dt alt-dy/dt]
    (define dx/dt (or alt-dx/dt (λ [[t : Real]] (df/dx xt t))))
    (define dy/dt (or alt-dy/dt (λ [[t : Real]] (df/dx yt t))))
    
    (λ [[t : Real]]
      (define dx (dx/dt t))
      (define dy (dy/dt t))

      (and dx dy
           (cond [(zero? dx) +inf.0]
                 [else (/ dy dx)])))))

(define parametric-dd/dtt : (-> (-> Real Real) (-> Real Real)
                                (Option (-> Real (Option Real))) (Option (-> Real (Option Real)))
                                (Option (-> Real (Option Real))) (Option (-> Real (Option Real)))
                                (-> Real (Option Real)))
  (lambda [xt yt alt-dx/dt alt-ddx/dtt alt-dy/dt alt-ddy/dtt]
    (define dx/dt (or alt-dx/dt (λ [[t : Real]] (df/dx xt t))))
    (define dy/dt (or alt-dy/dt (λ [[t : Real]] (df/dx xt t))))
    (define ddx/dtt (or alt-ddx/dtt (λ [[t : Real]] (ddf/dxx xt t))))
    (define ddy/dtt (or alt-ddy/dtt (λ [[t : Real]] (ddf/dxx xt t))))
    
    (λ [[t : Real]]
      (define dx (dx/dt t))
      (define dy (dy/dt t))
      (define ddx (ddx/dtt t))
      (define ddy (ddy/dtt t))

      (and dx ddx dy ddy
           (cond [(zero? dx) +inf.0]
                 [else (/ (- (* dx ddy) (* dy ddx))
                          (expt dx 3))])))))

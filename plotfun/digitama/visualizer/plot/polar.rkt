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
(define polar
  (lambda [#:id [id : (Option Symbol) #false]
           #:label [label : (U False Geo-Rich-Text Plot:Mark 'name) 'name]
           #:label-position [at-frac : Real (default-plot-visualizer-label-position)]
           #:label-position-range [rng-frac : (Pairof Real Real) (default-plot-visualizer-label-position-range)]
           #:label-placement [placement : Plot-Visualizer-Label-Placement (default-plot-visualizer-label-placement)]
           #:color [strk-color : (Option Color) #false]
           #:width [strk-width : (Option Real) #false]
           #:opacity [opacity : (Option Real) #false]
           #:dash [strk-dash : (Option Stroke-Dash+Offset) #false]
           ;#:safe-dr/dθ [alt-dr/dθ : (Option (-> Real (Option Real))) #false]
           ;#:safe-ddr/dθθ [alt-ddr/dθθ : (Option (-> Real (Option Real))) #false]
           [r : (-> Real (Option Number))]
           [θmin : Real 0] [θmax : Real 2pi]] : Plot-Visualizer
    (define safe-r (safe-real-function r))
    (define-values (points lx ty rx by) (polar-points safe-r θmin θmax 500))
    (define-values (xrange yrange) (plot-range-normalize lx rx #false #false))
    (define discrete-df/dx : (-> Real (Option Real)) (plot-lines-df/dx points))

    (define polar-realize : Plot-Visualizer-Realize
      (λ [idx total xmin xmax ymin ymax transform bg-color]
        (define-values (dots x y width height) (~cartesian2ds points xmin xmax ymin ymax transform))

        ;;; TODO: implement real polar dynamic angle
        (define dynamic-angle : (-> Real (Option Real)) (plot-function-pin-angle discrete-df/dx 0 placement))

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
                                      #:label (plot-function-mark-guard safe-r label [idx total θmin θmax xmin xmax ymin ymax at-frac rng-frac])
                                      #:pin-angle dynamic-angle
                                      #:gap-angle dynamic-angle]
                           dots)))

    (plot-visualizer polar-realize xrange yrange
                     (plot-lines-range points)
                     (and strk-color #true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define polar-points : (-> (-> Real Real) Real Real Positive-Index (Values (Listof Float-Complex) Flonum Flonum Flonum Flonum))
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

(define polar-df/dx : (-> (-> Real Real) (Option (-> Real (Option Real))) (-> Real (Option Real)))
  (lambda [rθ alt-dr/dθ]
    (define dr/dθ (or alt-dr/dθ (λ [[theta : Real]] (df/dx rθ theta))))
    
    (λ [[θ : Real]]
      (define r (rθ θ))
      (define dr (dr/dθ θ))

      (and (rational? r) dr
           (let* ([sinθ (sin θ)]
                  [cosθ (cos θ)]
                  [dx (- (* dr cosθ) (* r sinθ))]
                  [dy (+ (* dr sinθ) (* r cosθ))])
             (cond [(zero? dx) +inf.0]
                   [else (/ dy dx)]))))))

(define polar-ddf/dxx : (-> (-> Real Real) (Option (-> Real (Option Real))) (Option (-> Real (Option Real))) (-> Real (Option Real)))
  (lambda [rθ alt-dr/dθ alt-ddr/dθθ]
    (define dr/dθ (or alt-dr/dθ (λ [[theta : Real]] (df/dx rθ theta))))
    (define ddr/dθθ (or alt-ddr/dθθ (λ [[theta : Real]] (ddf/dxx rθ theta))))
    
    (λ [[θ : Real]]
      (define r (rθ θ))
      (define dr (dr/dθ θ))
      (define ddr (ddr/dθθ θ))

      (and (rational? r) dr ddr
           (let* ([sinθ (sin θ)]
                  [cosθ (cos θ)]
                  [dx (- (* dr cosθ) (* r sinθ))]
                  [dy (+ (* dr sinθ) (* r cosθ))]
                  [ddx (- (* ddr cosθ) (* 2 dr sinθ) (* r cosθ))]
                  [ddy (- (+ (* ddr sinθ) (* 2 dr cosθ)) (* r sinθ))])
             (cond [(zero? dx) +inf.0]
                   [else (/ (- (* dx ddy) (* dy ddx))
                            (expt dx 3))]))))))

#lang typed/racket/base

(provide (all-defined-out))

(require "../paint.rkt")
(require "../../paint.rkt")

(require "../convert.rkt")
(require "../unsafe/dc/path.rkt")

(require "../geometry/dot.rkt")
(require "../geometry/footprint.rkt")
(require "../geometry/bezier.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:bezier geo
  ([prints : Geo-Path-Clean-Prints]
   [tx : Flonum]
   [ty : Flonum]
   [closed? : Boolean])
  #:type-name Geo:Bezier
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-polybezier*
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           #:fill-rule [rule : Fill-Rule (default-fill-rule)]
           #:id [id : (Option Symbol) #false]
           #:scale [scale : Point2D 1.0]
           #:offset [offset : Complex 0.0+0.0i]
           #:window [window : (Option Point2D) #false]
           #:samples [samples : Index 200]
           #:close? [close? : Boolean #false]
           [path : (Listof (U Point2D (Listof Point2D)))]] : Geo:Bezier
    (define-values (prints lx ty rx by) (~point2ds* path offset scale))
    (define-values (xoff yoff width height x-stroke? y-stroke?) (point2d->window (or window +nan.0+nan.0i) lx ty rx by))
    (define clean-prints (geo-path-cleanse prints #:bezier-samples samples))
    (define-values (inkx inky inkw inkh) (geo-path-ink-box clean-prints))
    
    (create-geometry-object geo:bezier
                            #:with [id (geo-draw-bezier stroke pattern rule)
                                       (geo-shape-extent width height (+ inkx xoff) (+ inky yoff) inkw inkh)
                                       (geo-shape-outline stroke x-stroke? y-stroke?)]
                            clean-prints xoff yoff close?)))

(define geo-bezier
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           #:fill-rule [rule : Fill-Rule (default-fill-rule)]
           #:id [id : (Option Symbol) #false]
           #:scale [scale : Point2D 1.0]
           #:offset [offset : Complex 0.0+0.0i]
           #:window [window : (Option Point2D) #false]
           #:samples [samples : Index 200]
           #:close? [close? : Boolean #false]
           . [path : Point2D *]] : Geo:Bezier
    (geo-polybezier* #:id id #:stroke stroke #:fill pattern #:fill-rule rule
                     #:samples samples #:close? close?
                     #:scale scale #:offset offset #:window window
                     (list path))))

(define geo-bezier*
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           #:fill-rule [rule : Fill-Rule (default-fill-rule)]
           #:id [id : (Option Symbol) #false]
           #:scale [scale : Point2D 1.0]
           #:offset [offset : Complex 0.0+0.0i]
           #:window [window : (Option Point2D) #false]
           #:samples [samples : Index 200]
           #:close? [close? : Boolean #false]
           [path : (Listof Point2D)]] : Geo:Bezier
    (geo-polybezier* #:id id #:stroke stroke #:fill pattern #:fill-rule rule
                     #:samples samples #:close? close?
                     #:scale scale #:offset offset #:window window
                     (list path))))

(define geo-polybezier
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           #:fill-rule [rule : Fill-Rule (default-fill-rule)]
           #:id [id : (Option Symbol) #false]
           #:scale [scale : Point2D 1.0]
           #:offset [offset : Complex 0.0+0.0i]
           #:window [window : (Option Point2D) #false]
           #:samples [samples : Index 200]
           #:close? [close? : Boolean #false]
           . [path : (U Point2D (Listof Point2D)) *]] : Geo:Bezier
    (geo-polybezier* #:id id #:stroke stroke #:fill pattern #:fill-rule rule
                     #:samples samples #:close? close?
                     #:scale scale #:offset offset #:window window
                     path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-draw-bezier : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Fill-Rule Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill fill-rule]
    (λ [self cr x0 y0 width height]
      (when (and (geo:bezier? self)
                 (pair? (geo:bezier-prints self)))
        (dc_polyline* cr (+ x0 (geo:bezier-tx self)) (+ y0 (geo:bezier-ty self)) width height
                      (geo:bezier-prints self)
                      (geo-select-stroke-paint* alt-stroke)
                      (geo-select-fill-source alt-fill) fill-rule
                      (geo:bezier-closed? self))))))

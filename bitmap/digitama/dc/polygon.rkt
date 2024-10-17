#lang typed/racket/base

(provide (all-defined-out) Bitmap)
(provide (rename-out [bitmap-polyline bitmap-lines]
                     [bitmap-arrowhead bitmap-dart]
                     [bitmap-trapezium bitmap-trapezoid]))

(require digimon/metrics)
(require geofun/paint)
(require geofun/stroke)

(require geofun/digitama/base)
(require geofun/digitama/source)

(require geofun/digitama/geometry/dot)
(require geofun/digitama/geometry/constants)
(require geofun/digitama/unsafe/path)

(require geofun/digitama/geometry/polygon/quadrilateral)
(require geofun/digitama/geometry/polygon/pentagon)
(require geofun/digitama/geometry/polygon/hexagon)
(require geofun/digitama/geometry/polygon/arrow)

(require "../convert.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-polyline : (->* ((U Point2D (Listof Point2D)))
                               (Real Real #:scale Point2D #:window (Option Point2D) #:stroke Maybe-Stroke-Paint #:close? Boolean #:density Positive-Flonum)
                               Bitmap)
  (lambda [#:scale [scale 1.0] #:stroke [stroke (default-stroke-paint)] #:close? [close? #false] #:density [density (default-bitmap-density)]
           #:window [window #false]
           pts [dx 0.0] [dy 0.0]]
    (define-values (prints lx ty rx by) (~point2ds (if (list? pts) pts (list pts)) dx dy scale))
    (define-values (xoff yoff width height x-stroke? y-stroke?) (point2d->window (or window +nan.0+nan.0i) lx ty rx by))
    (define s : Stroke (stroke-paint->source stroke))
    (define-values (x0 y0 w h) (path-window-adjust xoff yoff width height (stroke-width s) x-stroke? y-stroke?))

    (draw-bitmap dc_polyline #:with [x0 y0 w h density #true] prints s close?)))

(define bitmap-polygon : (->* ((U Point2D (Listof Point2D)))
                              (Real Real #:scale Point2D #:stroke Maybe-Stroke-Paint #:fill Option-Fill-Paint #:fill-rule Symbol
                                    #:density Positive-Flonum #:window (Option Point2D))
                              Bitmap)
  (lambda [#:scale [scale 1.0] #:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)] #:fill-rule [rule (default-fill-rule)]
           #:density [density (default-bitmap-density)] #:window [window #false]
           pts [dx 0.0] [dy 0.0]]
    (define-values (prints lx ty rx by) (~point2ds (if (list? pts) pts (list pts)) dx dy scale))
    (define-values (xoff yoff width height x-stroke? y-stroke?) (point2d->window (or window +nan.0+nan.0i) lx ty rx by))
    (define s : (Option Stroke) (stroke-paint->source outline))
    (define-values (x0 y0 w h) (path-window-adjust xoff yoff width height (stroke-maybe-width s) x-stroke? y-stroke?))
    
    (draw-bitmap dc_polygon #:with [x0 y0 w h density #true]
                 prints s (fill-paint->source* pattern) rule)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-arrow : (->* (Real Real)
                            (Real #:shaft-thickness Real #:wing-angle (Option Real) #:radian? Boolean
                                  #:stroke Maybe-Stroke-Paint #:fill Option-Fill-Paint #:density Positive-Flonum)
                            Bitmap)
  (lambda [#:shaft-thickness [shaft-thickness -0.3] #:wing-angle [wing-angle #false] #:radian? [radian? #true]
           #:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)] #:density [density (default-bitmap-density)]
           head-radius shaft-length [start 0.0]]
    (define rhead : Nonnegative-Flonum (~length head-radius))
    (define-values (prints tx ty width height)
      (geo-arrow-metrics rhead (~radian start radian?)
                         (~length shaft-thickness rhead) (~length shaft-length rhead)
                         (and wing-angle (~radian wing-angle radian?))))
    (define s : (Option Stroke) (stroke-paint->source* outline))
    (define-values (x0 y0 w h) (path-window-adjust tx ty width height (stroke-maybe-width s) #true #false))
    
    (draw-bitmap dc_polygon* #:with [x0 y0 w h density #true]
                 prints s (fill-paint->source* pattern)
                 (default-fill-rule))))

(define bitmap-arrowhead : (->* (Real)
                                (Real #:wing-angle (Option Real) #:curve-ratio Flonum #:radian? Boolean
                                      #:stroke Maybe-Stroke-Paint #:fill Option-Fill-Paint #:density Positive-Flonum)
                                Bitmap)
  (lambda [#:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)] #:curve-ratio [curve-ratio 0.384]
           #:wing-angle [wing-angle #false] #:radian? [radian? #true] #:density [density (default-bitmap-density)]
           radius [start 0.0]]
    (define r : Nonnegative-Flonum (~length radius))
    (define-values (prints tx ty width height) (geo-dart-metrics r (~radian start radian?) (and wing-angle (~radian wing-angle radian?))))
    (define s : (Option Stroke) (stroke-paint->source* outline))
    (define-values (x0 y0 w h) (path-window-adjust tx ty width height (stroke-maybe-width s) #true #false))
    
    (draw-bitmap dc_polygon* #:with [x0 y0 w h density #true]
                 prints s (fill-paint->source* pattern)
                 (default-fill-rule))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-parallelogram : (-> Real Real Real
                                   [#:stroke Maybe-Stroke-Paint] [#:fill Option-Fill-Paint] [#:density Positive-Flonum] [#:radian? Boolean]
                                   Bitmap)
  (lambda [#:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)] #:density [density (default-bitmap-density)] #:radian? [radian? #true]
           width height angle]
    (define-values (flwidth flheight) (~size width height))
    
    (bitmap-polygon #:stroke outline #:fill pattern #:density density #:window +nan.0+nan.0i
                    (geo-parallelogram-vertices flwidth flheight (~cycle (~radian angle radian?) 2pi 0.0)))))

(define bitmap-rhombus : (-> Real Real [#:stroke Maybe-Stroke-Paint] [#:fill Option-Fill-Paint] [#:density Positive-Flonum] Bitmap)
  (lambda [width height #:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)] #:density [density (default-bitmap-density)]]
    (define-values (flwidth flheight) (~size width height))
    
    (bitmap-polygon #:stroke outline #:fill pattern #:density density #:window +nan.0+nan.0i
                    (geo-rhombus-vertices flwidth flheight))))

(define bitmap-house : (->* (Real Real) (Real #:stroke Maybe-Stroke-Paint #:fill Option-Fill-Paint #:density Positive-Flonum) Bitmap)
  (lambda [width height [t 0.618] #:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)] #:density [density (default-bitmap-density)]]
    (define-values (flwidth flheight) (~size width height))
    
    (bitmap-polygon #:stroke outline #:fill pattern #:density density #:window +nan.0+nan.0i
                    (geo-house-vertices flwidth flheight (real->double-flonum t)))))

(define bitmap-trapezium : (->* (Real Real) (Nonnegative-Real #:stroke Maybe-Stroke-Paint #:fill Option-Fill-Paint #:density Positive-Flonum) Bitmap)
  (lambda [width height [t 0.618] #:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)] #:density [density (default-bitmap-density)]]
    (define-values (flwidth flheight) (~size width height))
    
    (bitmap-polygon #:stroke outline #:fill pattern #:density density #:window +nan.0+nan.0i
                    (geo-isosceles-trapezium-vertices flwidth flheight (real->double-flonum t)))))

(define bitmap-keyboard : (->* (Real Real) (Nonnegative-Real #:stroke Maybe-Stroke-Paint #:fill Option-Fill-Paint #:density Positive-Flonum) Bitmap)
  (lambda [width height [t 0.618] #:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)] #:density [density (default-bitmap-density)]]
    (define-values (flwidth flheight) (~size width height))
    
    (bitmap-polygon #:stroke outline #:fill pattern #:density density #:window +nan.0+nan.0i
                    (geo-keyboard-vertices flwidth flheight (real->double-flonum t)))))

(define bitmap-hexagon-tile : (-> Real Real [#:stroke Maybe-Stroke-Paint] [#:fill Option-Fill-Paint] [#:density Positive-Flonum] Bitmap)
  (lambda [width height #:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)] #:density [density (default-bitmap-density)]]
    (define-values (flwidth flheight) (~size width height))
    
    (bitmap-polygon #:stroke outline #:fill pattern #:density density #:window +nan.0+nan.0i
                    (geo-hexagon-tile-vertices flwidth flheight))))

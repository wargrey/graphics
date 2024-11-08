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

(require geofun/digitama/geometry/polygon/quadrilateral)
(require geofun/digitama/geometry/polygon/pentagon)
(require geofun/digitama/geometry/polygon/hexagon)
(require geofun/digitama/geometry/polygon/arrow)

(require geofun/digitama/unsafe/dc/path)

(require "../convert.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-polyline
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (default-stroke-paint)]
           #:scale [scale : Point2D 1.0] #:close? [close? : Boolean #false] #:window [window : (Option Point2D) #false]
           #:density [density : Positive-Flonum (default-bitmap-density)]
           [pts : (U Point2D (Listof Point2D))] [dx : Real 0.0] [dy : Real 0.0]] : Bitmap
    (define-values (prints lx ty rx by) (~point2ds (if (list? pts) pts (list pts)) dx dy scale))
    (define-values (xoff yoff width height x-stroke? y-stroke?) (point2d->window (or window +nan.0+nan.0i) lx ty rx by))
    (define s : Stroke (stroke-paint->source stroke))
    (define-values (x0 y0 w h) (path-window-adjust xoff yoff width height (stroke-width s) x-stroke? y-stroke?))

    (draw-bitmap dc_polyline #:with [x0 y0 w h density #true] prints s close?)))

(define bitmap-polygon
  (lambda [#:stroke [outline : Maybe-Stroke-Paint (default-stroke-paint)] #:fill [pattern : Option-Fill-Paint (default-fill-paint)]
           #:fill-rule [rule : Fill-Rule (default-fill-rule)] #:close? [close? : Boolean #false]
           #:scale [scale : Point2D 1.0]  #:window [window : (Option Point2D) #false]
           #:density [density : Positive-Flonum (default-bitmap-density)]
           [pts : (U Point2D (Listof Point2D))] [dx : Real 0.0] [dy : Real 0.0]] : Bitmap
    (define-values (prints lx ty rx by) (~point2ds (if (list? pts) pts (list pts)) dx dy scale))
    (define-values (xoff yoff width height x-stroke? y-stroke?) (point2d->window (or window +nan.0+nan.0i) lx ty rx by))
    (define s : (Option Stroke) (stroke-paint->source outline))
    (define-values (x0 y0 w h) (path-window-adjust xoff yoff width height (stroke-maybe-width s) x-stroke? y-stroke?))
    
    (draw-bitmap dc_polygon #:with [x0 y0 w h density #true]
                 prints s (fill-paint->source* pattern) rule)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-arrow
  (lambda [#:shaft-thickness [shaft-thickness : Real -0.3] #:wing-angle [wing-angle : (Option Real) #false] #:radian? [radian? : Boolean #true]
           #:stroke [outline : Maybe-Stroke-Paint (default-stroke-paint)] #:fill [pattern : Option-Fill-Paint (default-fill-paint)]
           #:density [density : Positive-Flonum (default-bitmap-density)]
           [head-radius : Real] [shaft-length : Real] [start : Real 0.0]] : Bitmap
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

(define bitmap-arrowhead
  (lambda [#:stroke [outline : Maybe-Stroke-Paint (default-stroke-paint)] #:fill [pattern : Option-Fill-Paint (default-fill-paint)]
           #:wing-angle [wing-angle : (Option Real) #false] #:radian? [radian? : Boolean #true]
           #:density [density : Positive-Flonum (default-bitmap-density)]
           [radius : Real] [start : Real 0.0]] : Bitmap
    (define r : Nonnegative-Flonum (~length radius))
    (define-values (prints tx ty width height) (geo-dart-metrics r (~radian start radian?) (and wing-angle (~radian wing-angle radian?))))
    (define s : (Option Stroke) (stroke-paint->source* outline))
    (define-values (x0 y0 w h) (path-window-adjust tx ty width height (stroke-maybe-width s) #true #false))
    
    (draw-bitmap dc_polygon* #:with [x0 y0 w h density #true]
                 prints s (fill-paint->source* pattern)
                 (default-fill-rule))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-parallelogram
  (lambda [#:stroke [outline : Maybe-Stroke-Paint (default-stroke-paint)] #:fill [pattern : Option-Fill-Paint (default-fill-paint)]
           #:radian? [radian? : Boolean #true] #:density [density : Positive-Flonum (default-bitmap-density)]
           [width : Real] [height : Real] [angle : Real]] : Bitmap
    (define-values (flwidth flheight) (~size width height))
    
    (bitmap-polygon #:stroke outline #:fill pattern #:density density #:window +nan.0+nan.0i
                    (geo-parallelogram-vertices flwidth flheight (~wrap (~radian angle radian?) 2pi 0.0)))))

(define bitmap-rhombus
  (lambda [#:stroke [outline : Maybe-Stroke-Paint (default-stroke-paint)] #:fill [pattern : Option-Fill-Paint (default-fill-paint)]
           #:density [density : Positive-Flonum (default-bitmap-density)]
           [width : Real] [height : Real]] : Bitmap
    (define-values (flwidth flheight) (~size width height))
    
    (bitmap-polygon #:stroke outline #:fill pattern #:density density #:window +nan.0+nan.0i
                    (geo-rhombus-vertices flwidth flheight))))

(define bitmap-house
  (lambda [#:stroke [outline : Maybe-Stroke-Paint (default-stroke-paint)] #:fill [pattern : Option-Fill-Paint (default-fill-paint)]
           #:density [density : Positive-Flonum (default-bitmap-density)]
           [width : Real] [height : Real] [t : Real 0.618]] : Bitmap
    (define-values (flwidth flheight) (~size width height))
    
    (bitmap-polygon #:stroke outline #:fill pattern #:density density #:window +nan.0+nan.0i
                    (geo-house-vertices flwidth flheight (real->double-flonum t)))))

(define bitmap-trapezium
  (lambda [#:stroke [outline : Maybe-Stroke-Paint (default-stroke-paint)] #:fill [pattern : Option-Fill-Paint (default-fill-paint)]
           #:density [density : Positive-Flonum (default-bitmap-density)]
           [width : Real] [height : Real] [t : Nonnegative-Real 0.618]] : Bitmap
    (define-values (flwidth flheight) (~size width height))
    
    (bitmap-polygon #:stroke outline #:fill pattern #:density density #:window +nan.0+nan.0i
                    (geo-isosceles-trapezium-vertices flwidth flheight (real->double-flonum t)))))

(define bitmap-keyboard
  (lambda [#:stroke [outline : Maybe-Stroke-Paint (default-stroke-paint)] #:fill [pattern : Option-Fill-Paint (default-fill-paint)]
           #:density [density : Positive-Flonum (default-bitmap-density)]
           [width : Real] [height : Real] [t : Nonnegative-Real 0.618]] : Bitmap
    (define-values (flwidth flheight) (~size width height))
    
    (bitmap-polygon #:stroke outline #:fill pattern #:density density #:window +nan.0+nan.0i
                    (geo-keyboard-vertices flwidth flheight (real->double-flonum t)))))

(define bitmap-hexagon-tile
  (lambda [#:stroke [outline : Maybe-Stroke-Paint (default-stroke-paint)] #:fill [pattern : Option-Fill-Paint (default-fill-paint)]
           #:density [density : Positive-Flonum (default-bitmap-density)]
           [width : Real] [height : Real]] : Bitmap
    (define-values (flwidth flheight) (~size width height))
    
    (bitmap-polygon #:stroke outline #:fill pattern #:density density #:window +nan.0+nan.0i
                    (geo-hexagon-tile-vertices flwidth flheight))))

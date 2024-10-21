#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)
(require geofun/paint)

(require geofun/digitama/base)
(require geofun/digitama/source)
(require geofun/digitama/unsafe/dc/shape)

(require "../convert.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-square
  (lambda [#:stroke [outline : Maybe-Stroke-Paint (default-stroke-paint)] #:fill [pattern : Option-Fill-Paint (default-fill-paint)]
           #:vlines [vlines : (Listof Real) null] #:hlines [hlines : (Listof Real) null]
           #:filter [filter : Symbol (default-pattern-filter)] #:density [density : Positive-Flonum (default-bitmap-density)]
           [width : Real] [corner-radius : Real 0.0]] : Bitmap
    (define flvls : (Listof Flonum) (map real->double-flonum vlines))
    (define flhls : (Listof Flonum) (map real->double-flonum hlines))
    (define w : Nonnegative-Flonum (~length width))
    
    (if (zero? corner-radius)
        (draw-bitmap dc_rectangle #:with [w w density #true filter (stroke-paint->source* outline)]
                     [] [(fill-paint->source* pattern) flvls flhls])
        (draw-bitmap dc_rounded_rectangle #:with [w w density #true filter (stroke-paint->source* outline)]
                     [(~length corner-radius w)] [(fill-paint->source* pattern) flvls flhls]))))

(define bitmap-rectangle
  (lambda [#:stroke [outline : Maybe-Stroke-Paint (default-stroke-paint)] #:fill [pattern : Option-Fill-Paint (default-fill-paint)]
           #:vlines [vlines : (Listof Real) null] #:hlines [hlines : (Listof Real) null]
           #:filter [filter : Symbol (default-pattern-filter)] #:density [density : Positive-Flonum (default-bitmap-density)]
           [width : Real] [height : Real -0.618] [corner-radius : Real 0.0]] : Bitmap
    (define flvls : (Listof Flonum) (map real->double-flonum vlines))
    (define flhls : (Listof Flonum) (map real->double-flonum hlines))
    (define-values (w h) (~size width height))
    
    (if (zero? corner-radius)
        (draw-bitmap dc_rectangle #:with [w h density #true filter (stroke-paint->source* outline)]
                     [] [(fill-paint->source* pattern) flvls flhls])
        (draw-bitmap dc_rounded_rectangle #:with [w h density #true filter (stroke-paint->source* outline)]
                     [(~length corner-radius (min w h))] [(fill-paint->source* pattern) flvls flhls]))))

(define bitmap-circle
  (lambda [#:stroke [outline : Maybe-Stroke-Paint (default-stroke-paint)] #:fill [pattern : Option-Fill-Paint (default-fill-paint)]
           #:diameters [diameters : (Listof Real) null] #:radian? [radian? : Boolean #true]
           #:filter [filter : Symbol (default-pattern-filter)] #:density [density : Positive-Flonum (default-bitmap-density)]
           [radius : Real]] : Bitmap
    (define w : Nonnegative-Flonum (* (~length radius) 2.0))
    (define flrads : (Listof Flonum) (for/list ([d (in-list diameters)]) (~radian d radian?)))
    
    (draw-bitmap dc_ellipse #:with [w w density #true filter (stroke-paint->source* outline)]
                 [] [(fill-paint->source* pattern) flrads])))

(define bitmap-ellipse
  (lambda [#:stroke [outline : Maybe-Stroke-Paint (default-stroke-paint)] #:fill [pattern : Option-Fill-Paint (default-fill-paint)]
           #:diameters [diameters : (Listof Real) null] #:radian? [radian? : Boolean #true]
           #:filter [filter : Symbol (default-pattern-filter)] #:density [density : Positive-Flonum (default-bitmap-density)]
           [width : Real] [height : Real -0.618]] : Bitmap
    (define-values (w h) (~size width height))
    (define flrads : (Listof Flonum) (for/list ([d (in-list diameters)]) (~radian d radian?)))
    
    (draw-bitmap dc_ellipse #:with [w h density #true filter (stroke-paint->source* outline)]
                 [] [(fill-paint->source* pattern) flrads])))

(define bitmap-sector
  (lambda [#:stroke [outline : Maybe-Stroke-Paint (default-stroke-paint)] #:fill [pattern : Option-Fill-Paint (default-fill-paint)]
           #:ratio [ratio : Real 1.0] #:radian? [radian? : Boolean #true]
           #:filter [filter : Symbol (default-pattern-filter)] #:density [density : Positive-Flonum (default-bitmap-density)]
           [radius : Real] [start : Real] [end : Real]] : Bitmap
    (define w : Nonnegative-Flonum (* (~length radius) 2.0))
    (define h : Nonnegative-Flonum (if (> ratio 0.0) (max 0.0 (/ w ratio)) w))
    
    (draw-bitmap dc_arc #:with [w h density #true filter (stroke-paint->source* outline)]
                 [(~radian start radian?) (~radian end radian?)] [(fill-paint->source* pattern) #true])))

(define bitmap-arc
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (default-stroke-paint)] #:ratio [ratio : Real 1.0] #:radian? [radian? : Boolean #true]
           #:filter [filter : Symbol (default-pattern-filter)] #:density [density : Positive-Flonum (default-bitmap-density)]
           [radius : Real] [start : Real] [end : Real]] : Bitmap
    (define w : Nonnegative-Flonum (* (~length radius) 2.0))
    (define h : Nonnegative-Flonum (if (> ratio 0.0) (max 0.0 (/ w ratio)) w))
    
    (draw-bitmap dc_arc #:with [w h density #true filter (stroke-paint->source stroke)]
                 [(~radian start radian?) (~radian end radian?)] [#false #false])))

(define bitmap-regular-polygon
  (lambda [#:stroke [outline : Maybe-Stroke-Paint (default-stroke-paint)] #:fill [pattern : Option-Fill-Paint (default-fill-paint)]
           #:ratio [ratio : Real 1.0] #:radian? [radian? : Boolean #true] #:inscribed? [inscribed? : Boolean #false]
           #:filter [filter : Symbol (default-pattern-filter)] #:density [density : Positive-Flonum (default-bitmap-density)]
           [n : Integer] [radius : Real] [rotation : Real 0.0]] : Bitmap
    (if (and (index? n) (> n 0))
        (let* ([w (* (regular-polygon-radius->circumsphere-radius n (~length radius) (if inscribed? 'edge 'vertex)) 2.0)]
               [h (if (> ratio 0.0) (max 0.0 (/ w ratio)) w)])
          (draw-bitmap dc_regular_polygon #:with [w h density #true filter (stroke-paint->source* outline)]
                       [n (~radian rotation radian?)] [(fill-paint->source* pattern)]))
        (let* ([w (* (~length radius) 2.0)]
               [h (if (> ratio 0.0) (max 0.0 (/ w ratio)) w)])
          (draw-bitmap dc_ellipse #:with [w h density #true filter (stroke-paint->source* outline)]
                       [] [(fill-paint->source* pattern) null])))))

(define bitmap-hline
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (default-stroke-paint)]
           #:filter [filter : Symbol (default-pattern-filter)] #:density [density : Positive-Flonum (default-bitmap-density)]
           [width : Real] [height : Real]] : Bitmap
    (define-values (w h) (~size width height))
    (draw-bitmap dc_line #:with [w h density #true filter]
                 0.0 (* h 0.5) w 0.0 (stroke-paint->source stroke))))

(define bitmap-vline
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (default-stroke-paint)]
           #:filter [filter : Symbol (default-pattern-filter)] #:density [density : Positive-Flonum (default-bitmap-density)]
           [width : Real] [height : Real]] : Bitmap
    (define-values (w h) (~size width height))
    (draw-bitmap dc_line #:with [w h density #true filter]
                 (* w 0.5) 0.0 0.0 h (stroke-paint->source stroke))))

(define bitmap-diagonal
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (default-stroke-paint)]
           #:filter [filter : Symbol (default-pattern-filter)] #:density [density : Positive-Flonum (default-bitmap-density)]
           [width : Real] [height : Real]] : Bitmap
    (define-values (w h) (~size width height))
    (draw-bitmap dc_line #:with [w h density #true filter]
                 0.0 0.0 w h (stroke-paint->source stroke))))

(define bitmap-anti-diagonal
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (default-stroke-paint)]
           #:filter [filter : Symbol (default-pattern-filter)] #:density [density : Positive-Flonum (default-bitmap-density)]
           [width : Real] [height : Real]] : Bitmap
    (define-values (w h) (~size width height))
    (draw-bitmap dc_line #:with [w h density #true filter]
                 0.0 h w (- h) (stroke-paint->source stroke))))

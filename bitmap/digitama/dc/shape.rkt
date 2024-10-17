#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)
(require geofun/paint)

(require geofun/digitama/base)
(require geofun/digitama/source)
(require geofun/digitama/unsafe/dc/shape)

(require "../convert.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-square : (->* (Real)
                             (Real #:stroke Maybe-Stroke-Paint #:fill Option-Fill-Paint #:density Positive-Flonum
                                   #:vlines (Listof Real) #:hlines (Listof Real))
                             Bitmap)
  (lambda [#:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)]
           #:vlines [vlines null] #:hlines [hlines null] #:density [density (default-bitmap-density)]
           width [corner-radius 0.0]]
    (define flvls : (Listof Flonum) (map real->double-flonum vlines))
    (define flhls : (Listof Flonum) (map real->double-flonum hlines))
    (define w : Nonnegative-Flonum (~length width))
    
    (if (zero? corner-radius)
        (draw-bitmap dc_rectangle #:with [w w density #true (stroke-paint->source* outline)]
                     [] [(fill-paint->source* pattern) flvls flhls])
        (draw-bitmap dc_rounded_rectangle #:with [w w density #true (stroke-paint->source* outline)]
                     [(~length corner-radius w)] [(fill-paint->source* pattern) flvls flhls]))))

(define bitmap-rectangle : (->* (Real)
                                (Real Real #:stroke Maybe-Stroke-Paint #:fill Option-Fill-Paint #:density Positive-Flonum
                                      #:vlines (Listof Real) #:hlines (Listof Real))
                                Bitmap)
  (lambda [#:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)]
           #:vlines [vlines null] #:hlines [hlines null] #:density [density (default-bitmap-density)]
           width [height -0.618] [corner-radius 0.0]]
    (define flvls : (Listof Flonum) (map real->double-flonum vlines))
    (define flhls : (Listof Flonum) (map real->double-flonum hlines))
    (define-values (w h) (~size width height))
    
    (if (zero? corner-radius)
        (draw-bitmap dc_rectangle #:with [w h density #true (stroke-paint->source* outline)]
                     [] [(fill-paint->source* pattern) flvls flhls])
        (draw-bitmap dc_rounded_rectangle #:with [w h density #true (stroke-paint->source* outline)]
                     [(~length corner-radius (min w h))] [(fill-paint->source* pattern) flvls flhls]))))

(define bitmap-circle : (-> Real [#:stroke Maybe-Stroke-Paint] [#:fill Option-Fill-Paint]
                            [#:diameters (Listof Real)] [#:radian? Boolean] [#:density Positive-Flonum]
                            Bitmap)
  (lambda [radius #:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)]
                  #:diameters [diameters null] #:radian? [radian? #true] #:density [density (default-bitmap-density)]]
    (define w : Nonnegative-Flonum (* (~length radius) 2.0))
    (define flrads : (Listof Flonum) (for/list ([d (in-list diameters)]) (~radian d radian?)))
    
    (draw-bitmap dc_ellipse #:with [w w density #true (stroke-paint->source* outline)]
                 [] [(fill-paint->source* pattern) flrads])))

(define bitmap-ellipse : (->* (Real)
                              (Real #:stroke Maybe-Stroke-Paint #:fill Option-Fill-Paint #:diameters (Listof Real) #:radian? Boolean #:density Positive-Flonum)
                              Bitmap)
  (lambda [#:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)]
           #:diameters [diameters null] #:radian? [radian? #true] #:density [density (default-bitmap-density)]
           width [height -0.618]]
    (define-values (w h) (~size width height))
    (define flrads : (Listof Flonum) (for/list ([d (in-list diameters)]) (~radian d radian?)))
    
    (draw-bitmap dc_ellipse #:with [w h density #true (stroke-paint->source* outline)]
                 [] [(fill-paint->source* pattern) flrads])))

(define bitmap-sector : (->* (Real Real Real)
                             (#:ratio Real #:stroke Maybe-Stroke-Paint #:fill Option-Fill-Paint #:radian? Boolean #:density Positive-Flonum)
                             Bitmap)
  (lambda [#:ratio [ratio 1.0] #:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)]
           #:radian? [radian? #true] #:density [density (default-bitmap-density)]
           radius start end]
    (define w : Nonnegative-Flonum (* (~length radius) 2.0))
    (define h : Nonnegative-Flonum (if (> ratio 0.0) (max 0.0 (/ w ratio)) w))
    
    (draw-bitmap dc_arc #:with [w h density #true (stroke-paint->source* outline)]
                 [(~radian start radian?) (~radian end radian?)] [(fill-paint->source* pattern) #true])))

(define bitmap-arc : (->* (Real Real Real) (#:ratio Real #:stroke Maybe-Stroke-Paint #:radian? Boolean #:density Positive-Flonum) Bitmap)
  (lambda [#:ratio [ratio 1.0] #:stroke [stroke (default-stroke-paint)] #:radian? [radian? #true] #:density [density (default-bitmap-density)]
           radius start end]
    (define w : Nonnegative-Flonum (* (~length radius) 2.0))
    (define h : Nonnegative-Flonum (if (> ratio 0.0) (max 0.0 (/ w ratio)) w))
    
    (draw-bitmap dc_arc #:with [w h density #true (stroke-paint->source stroke)]
                 [(~radian start radian?) (~radian end radian?)] [#false #false])))

(define bitmap-regular-polygon : (->* (Integer Real)
                                      (Real #:stroke Maybe-Stroke-Paint #:fill Option-Fill-Paint #:ratio Real
                                            #:radian? Boolean #:inscribed? Boolean #:density Positive-Flonum)
                                      Bitmap)
  (lambda [#:ratio [ratio 1.0] #:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)]
           #:radian? [radian? #true] #:inscribed? [inscribed? #false] #:density [density (default-bitmap-density)]
           n radius [rotation 0.0]]
    (if (and (index? n) (> n 0))
        (let* ([w (* (regular-polygon-radius->circumsphere-radius n (~length radius) (if inscribed? 'edge 'vertex)) 2.0)]
               [h (if (> ratio 0.0) (max 0.0 (/ w ratio)) w)])
          (draw-bitmap dc_regular_polygon #:with [w h density #true (stroke-paint->source* outline)]
                       [n (~radian rotation radian?)] [(fill-paint->source* pattern)]))
        (let* ([w (* (~length radius) 2.0)]
               [h (if (> ratio 0.0) (max 0.0 (/ w ratio)) w)])
          (draw-bitmap dc_ellipse #:with [w h density #true (stroke-paint->source* outline)]
                       [] [(fill-paint->source* pattern) null])))))

(define bitmap-hline : (->* (Real Real) (#:stroke Maybe-Stroke-Paint #:density Positive-Flonum) Bitmap)
  (lambda [width height #:stroke [stroke (default-stroke-paint)] #:density [density (default-bitmap-density)]]
    (define-values (w h) (~size width height))
    (draw-bitmap dc_line #:with [w h density #true]
                 0.0 (* h 0.5) w 0.0 (stroke-paint->source stroke))))

(define bitmap-vline : (->* (Real Real) (#:stroke Maybe-Stroke-Paint #:density Positive-Flonum) Bitmap)
  (lambda [width height #:stroke [stroke (default-stroke-paint)] #:density [density (default-bitmap-density)]]
    (define-values (w h) (~size width height))
    (draw-bitmap dc_line #:with [w h density #true]
                 (* w 0.5) 0.0 0.0 h (stroke-paint->source stroke))))

(define bitmap-diagonal : (->* (Real Real) (#:stroke Maybe-Stroke-Paint #:density Positive-Flonum) Bitmap)
  (lambda [width height #:stroke [stroke (default-stroke-paint)] #:density [density (default-bitmap-density)]]
    (define-values (w h) (~size width height))
    (draw-bitmap dc_line #:with [w h density #true]
                 0.0 0.0 w h (stroke-paint->source stroke))))

(define bitmap-anti-diagonal : (->* (Real Real) (#:stroke Maybe-Stroke-Paint #:density Positive-Flonum) Bitmap)
  (lambda [width height #:stroke [stroke (default-stroke-paint)] #:density [density (default-bitmap-density)]]
    (define-values (w h) (~size width height))
    (draw-bitmap dc_line #:with [w h density #true]
                 0.0 h w (- h) (stroke-paint->source stroke))))

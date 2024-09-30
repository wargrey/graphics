#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require "../polygon.rkt")
(require "../../geometry/constants.rkt")
(require "../../geometry/polygon/quadrilateral.rkt")

(require "../../../paint.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-parallelogram : (-> Real Real Real
                                [#:id (Option Symbol)] [#:stroke Maybe-Stroke-Paint] [#:fill Maybe-Fill-Paint] [#:radian? Boolean]
                                Geo:Polygon)
  (lambda [#:id [id #false] #:stroke [outline (void)] #:fill [pattern (void)] #:radian? [radian? #true]
           width height angle]
    (define-values (flwidth flheight) (~size width height))
    
    (geo-polygon #:id (or id (gensym 'geo:polygon:parallelogram:)) #:stroke outline #:fill pattern #:window +nan.0+nan.0i
                 (geo-parallelogram-vertices flwidth flheight (~cycle (~radian angle radian?) 2pi 0.0)))))

(define geo-rhombus : (-> Real Real [#:id (Option Symbol)] [#:stroke Maybe-Stroke-Paint] [#:fill Maybe-Fill-Paint] Geo:Polygon)
  (lambda [width height #:id [id #false] #:stroke [outline (void)] #:fill [pattern (void)]]
    (define-values (flwidth flheight) (~size width height))
    
    (geo-polygon #:id (or id (gensym 'geo:polygon:rhombus:)) #:stroke outline #:fill pattern #:window +nan.0+nan.0i
                 (geo-rhombus-vertices flwidth flheight))))

(define geo-trapezium : (->* (Real Real) (Nonnegative-Real #:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint) Geo:Polygon)
  (lambda [width height [t 0.618] #:id [id #false] #:stroke [outline (void)] #:fill [pattern (void)]]
    (define-values (flwidth flheight) (~size width height))
    
    (geo-polygon #:id (or id (gensym 'geo:polygon:trapezium:)) #:stroke outline #:fill pattern #:window +nan.0+nan.0i
                 (geo-isosceles-trapezium-vertices flwidth flheight (real->double-flonum t)))))

(define geo-keyboard : (->* (Real Real) (Nonnegative-Real #:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint) Geo:Polygon)
  (lambda [width height [t 0.618] #:id [id #false] #:stroke [outline (void)] #:fill [pattern (void)]]
    (define-values (flwidth flheight) (~size width height))
    
    (geo-polygon #:id (or id (gensym 'geo:polygon:trapezium:)) #:stroke outline #:fill pattern #:window +nan.0+nan.0i
                 (geo-keyboard-vertices flwidth flheight (real->double-flonum t)))))

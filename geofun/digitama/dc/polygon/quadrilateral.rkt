#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)
(require digimon/metrics)

(require "../polygon.rkt")
(require "../../geometry/constants.rkt")
(require "../../geometry/polygon/quadrilateral.rkt")

(require "../../../paint.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-parallelogram : (-> Real Real Real
                                [#:id (Option Symbol)] [#:stroke Maybe-Stroke-Paint] [#:fill Maybe-Fill-Paint]
                                Geo:Polygon)
  (lambda [#:id [id #false] #:stroke [outline (void)] #:fill [pattern (void)]
           width height angle]
    (define-values (flwidth flheight) (~extent width height))
    
    (geo-polygon #:id (or id (gensym 'geo:polygon:parallelogram:)) #:stroke outline #:fill pattern #:window +nan.0+nan.0i
                 (geo-parallelogram-vertices flwidth flheight (~wrap (real->double-flonum angle) 2pi 0.0)))))

(define geo-rhombus : (->* (Real Real)
                           (Real #:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint)
                           Geo:Polygon)
  (lambda [width height [rotation +nan.0] #:id [id #false] #:stroke [outline (void)] #:fill [pattern (void)]]
    (define-values (flwidth flheight) (~extent width height))
    
    (geo-polygon #:id (or id (gensym 'geo:polygon:rhombus:)) #:stroke outline #:fill pattern #:window +nan.0+nan.0i
                 (cond [(nan? rotation) (geo-rhombus-vertices flwidth flheight)]
                       [else (geo-rhombus-vertices flwidth flheight (real->double-flonum rotation) 0.0+0.0i)]))))

(define geo-trapezium : (->* (Real Real) (Nonnegative-Real #:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint) Geo:Polygon)
  (lambda [width height [t 0.618] #:id [id #false] #:stroke [outline (void)] #:fill [pattern (void)]]
    (define-values (flwidth flheight) (~extent width height))
    
    (geo-polygon #:id (or id (gensym 'geo:polygon:trapezium:)) #:stroke outline #:fill pattern #:window +nan.0+nan.0i
                 (geo-isosceles-trapezium-vertices flwidth flheight (real->double-flonum t)))))

(define geo-keyboard : (->* (Real Real) (Nonnegative-Real #:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint) Geo:Polygon)
  (lambda [width height [t 0.618] #:id [id #false] #:stroke [outline (void)] #:fill [pattern (void)]]
    (define-values (flwidth flheight) (~extent width height))
    
    (geo-polygon #:id (or id (gensym 'geo:polygon:trapezium:)) #:stroke outline #:fill pattern #:window +nan.0+nan.0i
                 (geo-keyboard-vertices flwidth flheight (real->double-flonum t)))))

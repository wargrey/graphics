#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

(require "../polygon.rkt")
(require "../../paint/self.rkt")
(require "../../geometry/constants.rkt")
(require "../../geometry/polygon/quadrilateral.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-parallelogram
  (lambda [#:id [id : (Option Symbol) #false]
           #:stroke [outline : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           [width : Real-Length] [height : Length+%]
           [angle : Real] [unit : Angle-Unit 'rad]] : Geo:Polygon
    (define-values (flwidth flheight) (~extent width height))
    
    (geo-polygon #:id (or id (gensym 'geo:polygon:parallelogram:)) #:stroke outline #:fill pattern #:window +nan.0+nan.0i
                 (geo-parallelogram-vertices flwidth flheight (~wrap (~rad angle unit) 2pi 0.0)))))

(define geo-rhombus
  (lambda [#:id [id : (Option Symbol) #false]
           #:stroke [outline : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           [width : Real-Length] [height : Length+%]
           [rotation : Real +nan.0] [unit : Angle-Unit 'rad]] : Geo:Polygon
    (define-values (flwidth flheight) (~extent width height))
    
    (geo-polygon #:id (or id (gensym 'geo:polygon:rhombus:)) #:stroke outline #:fill pattern #:window +nan.0+nan.0i
                 (cond [(rational? rotation) (geo-rhombus-vertices flwidth flheight (~rad rotation unit) 0.0+0.0i)]
                       [else (geo-rhombus-vertices flwidth flheight)]))))

(define geo-trapezium : (->* (Real-Length Length+%)
                             (Nonnegative-Real #:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint)
                             Geo:Polygon)
  (lambda [width height [t 0.618] #:id [id #false] #:stroke [outline (void)] #:fill [pattern (void)]]
    (define-values (flwidth flheight) (~extent width height))
    
    (geo-polygon #:id (or id (gensym 'geo:polygon:trapezium:)) #:stroke outline #:fill pattern #:window +nan.0+nan.0i
                 (geo-isosceles-trapezium-vertices flwidth flheight (real->double-flonum t)))))

(define geo-keyboard : (->* (Real-Length Length+%)
                            (Nonnegative-Real #:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint)
                            Geo:Polygon)
  (lambda [width height [t 0.618] #:id [id #false] #:stroke [outline (void)] #:fill [pattern (void)]]
    (define-values (flwidth flheight) (~extent width height))
    
    (geo-polygon #:id (or id (gensym 'geo:polygon:trapezium:)) #:stroke outline #:fill pattern #:window +nan.0+nan.0i
                 (geo-keyboard-vertices flwidth flheight (real->double-flonum t)))))

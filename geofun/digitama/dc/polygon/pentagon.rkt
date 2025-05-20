#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)
(require digimon/constant)

(require "../polygon.rkt")
(require "../../geometry/polygon/pentagon.rkt")

(require "../../../paint.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-house : (->* (Real Real) (Real #:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint) Geo:Polygon)
  (lambda [width height [t 0.618] #:id [id #false] #:stroke [outline (void)] #:fill [pattern (void)]]
    (define-values (flwidth flheight) (~extent width height))
    
    (geo-polygon #:id (or id (gensym 'geo:polygon:pentagon:)) #:stroke outline #:fill pattern #:window +nan.0+nan.0i
                 (geo-house-vertices flwidth flheight (real->double-flonum t)))))

(define geo-star
  (lambda [#:stroke [outline : Maybe-Stroke-Paint (void)] #:fill [pattern : Maybe-Fill-Paint (void)]
           #:id [id : (Option Symbol) #false] #:radian? [radian? : Boolean #true] #:inscribed? [inscribed? : Boolean #false]
           [radius : Real] [rotation : Real -pi/2]] : Geo:Regular-Polygon
    (geo-star-polygon #:id (or id (gensym 'geo:polygon:star:)) #:stroke outline #:fill pattern
                      #:inscribed? inscribed? #:radian? radian?
                      5 2 radius rotation)))

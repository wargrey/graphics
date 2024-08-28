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
    
    (geo-polygon #:id (or id (gensym 'geo:polygon:parallelogram:)) #:stroke outline #:fill pattern #:window -1.0-1.0i
                 (geo-parallelogram-vertices flwidth flheight (~cycle (~radian angle radian?) 2pi 0.0)))))

(define geo-rhombus : (-> Real Real [#:id (Option Symbol)] [#:stroke Maybe-Stroke-Paint] [#:fill Maybe-Fill-Paint] Geo:Polygon)
  (lambda [width height #:id [id #false] #:stroke [outline (void)] #:fill [pattern (void)]]
    (define-values (flwidth flheight) (~size width height))
    
    (geo-polygon #:id (or id (gensym 'geo:polygon:rhombus:)) #:stroke outline #:fill pattern #:window -1.0-1.0i
                 (geo-rhombus-vertices flwidth flheight))))

  
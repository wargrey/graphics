#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require "../polygon.rkt")
(require "../../geometry/polygon/hexagon.rkt")

(require "../../../paint.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-hexagon-tile : (-> Real Real [#:id (Option Symbol)] [#:stroke Maybe-Stroke-Paint] [#:fill Maybe-Fill-Paint] Geo:Polygon)
  (lambda [width height #:id [id #false] #:stroke [outline (void)] #:fill [pattern (void)]]
    (define-values (flwidth flheight) (~size width height))
    
    (geo-polygon #:id (or id (gensym 'geo:polygon:hexagon:)) #:stroke outline #:fill pattern #:window +nan.0+nan.0i
                 (geo-hexagon-tile-vertices flwidth flheight))))

  
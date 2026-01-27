#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

(require "../polygon.rkt")
(require "../../paint/self.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-star
  (lambda [#:stroke [outline : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           #:id [id : (Option Symbol) #false]
           #:inscribed? [inscribed? : Boolean #false]
           [radius : Real] [rotation : Real -pi/2]] : Geo:Regular-Polygon
    (geo-star-polygon #:id (or id (gensym 'geo:polygon:star:)) #:stroke outline #:fill pattern
                      #:inscribed? inscribed?
                      5 2 radius rotation)))

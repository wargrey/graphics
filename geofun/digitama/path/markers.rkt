#lang typed/racket/base

(provide (all-defined-out))
(provide (struct-out geo-marker))
(provide  Geo-Marker-Name Option-Geo-Marker Maybe-Geo-Marker Geo-Tip-Placement)

(provide make-geo:mrk:arrow the-arrow.mrk the-generalization.mrk)
(provide make-geo:mrk:diamond the-aggregation.mrk the-composition.mrk)
(provide make-geo:mrk:dot the-dot.mrk the-odot.mrk the-pixel.mrk the-bullet.mrk the-circle.mrk)

(provide (rename-out [the-dot.mrk the-point.mrk]))

(require racket/case)

(require "marker/self.rkt")
(require "marker/dot.rkt")
(require "marker/arrow.rkt")
(require "marker/diamond.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-marker-filter : (-> Option-Geo-Marker (Option geo-marker))
  (lambda [self]
    (and self
         (cond [(geo-marker? self) self]
               [else (geo-marker-from-name self)]))))

(define geo-marker-from-name : (case-> [Geo-Marker-Name -> geo-marker]
                                       [Symbol -> (Option geo-marker)])
  (lambda [name]
    (case/eq name
     [(arrow) the-arrow.mrk]
     [(diamond) the-aggregation.mrk]
     [(bullet) the-bullet.mrk]
     [(circle) the-circle.mrk]
     [(triangle) the-generalization.mrk]
     [(dot point) the-dot.mrk]
     [(odot) the-odot.mrk]
     [(pixel) the-pixel.mrk]
     [else #false])))

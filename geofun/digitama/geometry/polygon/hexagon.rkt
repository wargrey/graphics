#lang typed/racket/base

(provide (all-defined-out))

(require "../dot.rkt")
(require "../constants.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Hexagon-Vertices (List Float-Complex Float-Complex Float-Complex Float-Complex Float-Complex Float-Complex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-hexagon-tile-vertices : (-> Nonnegative-Flonum Nonnegative-Flonum Hexagon-Vertices)
  (lambda [width height]
    (if (>= width height)
        (let ([25%w (* width 0.25)]
              [75%w (* width 0.75)]
              [50%h (* height 0.5)])
          (list (make-rectangular 25%w 0.0) (make-rectangular 75%w 0.0)
                (make-rectangular width 50%h) (make-rectangular 75%w height)
                (make-rectangular 25%w height) (make-rectangular 0.0 50%h)))
        (let ([25%h (* width 0.25)]
              [75%h (* width 0.75)]
              [50%w (* height 0.5)])
          (list (make-rectangular 50%w 0.0) (make-rectangular width 25%h)
                (make-rectangular width 75%h) (make-rectangular 50%w height)
                (make-rectangular 0.0 75%h) (make-rectangular 0.0 25%h))))))

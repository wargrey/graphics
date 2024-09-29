#lang typed/racket/base

(provide (all-defined-out))

(require "../dot.rkt")
(require "../constants.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Pentagon-Vertices (List Float-Complex Float-Complex Float-Complex Float-Complex Float-Complex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-house-vertices : (-> Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Pentagon-Vertices)
  (lambda [width height ratio]
    (define 50%w : Nonnegative-Flonum (* width 0.5))
    (define wtop : Nonnegative-Flonum (* height (max (- 1.0 ratio) 0.0)))
    
    (list (make-rectangular 0.0 wtop) (make-rectangular 50%w 0.0) (make-rectangular width wtop)
          (make-rectangular width height) (make-rectangular 0.0 height))))

(define geo-invhouse-vertices : (-> Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Pentagon-Vertices)
  (lambda [width height ratio]
    (define 50%w : Nonnegative-Flonum (* width 0.5))
    (define wbtm : Nonnegative-Flonum (* height ratio))
    
    (list 0.0+0.0i (make-rectangular width 0.0)
          (make-rectangular width wbtm) (make-rectangular 50%w height) (make-rectangular 0.0 wbtm))))

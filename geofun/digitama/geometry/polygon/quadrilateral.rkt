#lang typed/racket/base

(provide (all-defined-out))

(require "../dot.rkt")
(require "../constants.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Quadrilateral-Vertices (List Point2D Point2D Point2D Point2D))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-parallelogram-vertices : (-> Nonnegative-Flonum Nonnegative-Flonum Flonum Quadrilateral-Vertices)
  (lambda [width height normalized.rad]
    (define min-angle : Flonum (atan height width))

    (if (or (< min-angle normalized.rad (- pi min-angle))
            (< (+ pi min-angle) normalized.rad (- 2pi min-angle)))
        (let ([x (/ height (tan normalized.rad))])
          (if (>= x 0.0)
              (list (cons x 0.0) (cons width 0.0) (cons (- width x) height) (cons 0.0 height))
              (list (cons x 0.0) (cons (+ width x x) 0.0) (cons (+ width x) height) (cons 0.0 height))))
        
        ; as if it were rotated by 90 degrees
        (let ([x (* width (tan normalized.rad))])
          (if (>= x 0.0)
              (list (cons 0.0 x) (cons 0.0 height) (cons width (- height x)) (cons width 0.0))
              (list (cons 0.0 x) (cons 0.0 (+ height x x)) (cons width (+ height x)) (cons width 0.0)))))))

(define geo-rhombus-vertices : (-> Nonnegative-Flonum Nonnegative-Flonum Quadrilateral-Vertices)
  (lambda [width height]
    (define w/2 : Nonnegative-Flonum (* width 0.5))
    (define h/2 : Nonnegative-Flonum (* height 0.5))

    (list (cons w/2 0.0) (cons width h/2)
          (cons w/2 height) (cons 0.0 h/2))))

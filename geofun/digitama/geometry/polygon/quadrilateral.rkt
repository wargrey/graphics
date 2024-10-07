#lang typed/racket/base

(provide (all-defined-out))

(require "../dot.rkt")
(require "../constants.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Quadrilateral-Vertices (List Float-Complex Float-Complex Float-Complex Float-Complex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-parallelogram-vertices : (-> Nonnegative-Flonum Nonnegative-Flonum Flonum Quadrilateral-Vertices)
  (lambda [width height normalized.rad]
    (define min-angle : Flonum (atan height width))

    (if (or (< min-angle normalized.rad (- pi min-angle))
            (< (+ pi min-angle) normalized.rad (- 2pi min-angle)))
        (let ([x (/ height (tan normalized.rad))])
          (if (>= x 0.0)
              (list (make-rectangular x 0.0) (make-rectangular width 0.0)
                    (make-rectangular (- width x) height) (make-rectangular 0.0 height))
              (list (make-rectangular x 0.0) (make-rectangular (+ width x x) 0.0)
                    (make-rectangular (+ width x) height) (make-rectangular 0.0 height))))
        
        ; as if it were rotated by 90 degrees
        (let ([x (* width (tan normalized.rad))])
          (if (>= x 0.0)
              (list (make-rectangular 0.0 x) (make-rectangular 0.0 height)
                    (make-rectangular width (- height x)) (make-rectangular width 0.0))
              (list (make-rectangular 0.0 x) (make-rectangular 0.0 (+ height x x))
                    (make-rectangular width (+ height x)) (make-rectangular width 0.0)))))))

(define geo-rhombus-vertices : (-> Nonnegative-Flonum Nonnegative-Flonum Quadrilateral-Vertices)
  (lambda [width height]
    (define w/2 : Nonnegative-Flonum (* width 0.5))
    (define h/2 : Nonnegative-Flonum (* height 0.5))

    (list (make-rectangular w/2 0.0) (make-rectangular width h/2)
          (make-rectangular w/2 height) (make-rectangular 0.0 h/2))))

(define geo-keyboard-vertices : (-> Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Quadrilateral-Vertices)
  (lambda [width height left/right]
    (if (<= left/right 1.0)
        (let* ([left (* height left/right)]
               [off (- height left)])
          (list (make-rectangular 0.0 off) (make-rectangular width 0.0)
                (make-rectangular width height) (make-rectangular 0.0 height)))
        (let* ([right (/ height left/right)]
               [off (- height right)])
          (list 0.0+0.0i (make-rectangular width off)
                (make-rectangular width height) (make-rectangular 0.0 height))))))

(define geo-isosceles-trapezium-vertices : (-> Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Quadrilateral-Vertices)
  (lambda [width height upper/lower]
    (if (<= upper/lower 1.0)
        (let* ([upper (* width upper/lower)]
               [off (* (- width upper) 0.5)])
          (list (make-rectangular off 0.0) (make-rectangular (+ off upper) 0.0)
                (make-rectangular width height) (make-rectangular 0.0 height)))
        (let* ([lower (/ width upper/lower)]
               [off (* (- width lower) 0.5)])
          (list 0.0+0.0i (make-rectangular width 0.0)
                (make-rectangular (+ off lower) height) (make-rectangular off height))))))

(define geo-poor-hourglass-vertices : (-> Nonnegative-Flonum Nonnegative-Flonum Quadrilateral-Vertices)
  (lambda [width height]
    (list 0.0+0.0i (make-rectangular width 0.0)
          (make-rectangular 0.0 height) (make-rectangular width height))))

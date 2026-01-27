#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/geometry/polygon/quadrilateral)
(require geofun/digitama/geometry/polygon/pentagon)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; quadrilaterals
(define dia-keyboard-vertices : (-> Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Quadrilateral-Vertices)
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

(define dia-hourglass-vertices : (-> Nonnegative-Flonum Nonnegative-Flonum Quadrilateral-Vertices)
  (lambda [width height]
    (list 0.0+0.0i (make-rectangular width 0.0)
          (make-rectangular 0.0 height) (make-rectangular width height))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pentagons
(define dia-house-vertices : (-> Nonnegative-Flonum Nonnegative-Flonum Flonum Pentagon-Vertices)
  (lambda [width height ratio]
    (define 50%w : Nonnegative-Flonum (* width 0.5))

    (if (>= ratio 0.0)
        (let ([wtop (* height (max (- 1.0 ratio) 0.0))])
          (list (make-rectangular 0.0 wtop) (make-rectangular 50%w 0.0) (make-rectangular width wtop)
                (make-rectangular width height) (make-rectangular 0.0 height)))
        (let ([wbtm (* height (- ratio))])    
          (list 0.0+0.0i (make-rectangular width 0.0)
                (make-rectangular width wbtm) (make-rectangular 50%w height) (make-rectangular 0.0 wbtm))))))

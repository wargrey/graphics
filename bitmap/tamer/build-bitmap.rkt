#lang typed/racket/base

(require images/flomap)
(require "../base.rkt")

(define xy->a : (-> Integer Integer Real) (λ [x y] (/ (+ x y) 200)))
(define xy->r : (-> Integer Integer Real) (λ [x y] (/ (+ (- 100 x) y) 200)))
(define xy->g : (-> Integer Integer Real) (λ [x y] (/ (+ (- 100 x) (- 100 y)) 200)))
(define xy->b : (-> Integer Integer Real) (λ [x y] (/ (+ x (- 100 y)) 200)))

(define xy->vs (λ [[x : Integer] [y : Integer]] (vector (xy->a x y) (xy->r x y) (xy->g x y) (xy->b x y))))
(define xy->argb (λ [[x : Integer] [y : Integer]] (values (xy->a x y) (xy->r x y) (xy->g x y) (xy->b x y))))

(flomap->bitmap (build-flomap* 4 100 100 xy->vs))
(build-bitmap 100 100 xy->argb #:alpha-multiplied? #true)
(build-bitmap 100 100 xy->argb #:alpha-multiplied? #false)
 
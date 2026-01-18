#lang typed/racket/base

(provide (all-defined-out))

(require "constants.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type 2D-Radius-Type (U 'vertex 'edge))
(define-type 3D-Radius-Type (U 'face 2D-Radius-Type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define regular-polygon-radius->circumsphere-radius : (-> Positive-Index Nonnegative-Flonum 2D-Radius-Type Nonnegative-Flonum)
  (lambda [n R type]
    ; r = Rcos(pi/n)
    (cond [(eq? type 'vertex) R]
          [else (max (/ R (cos (/ pi (exact->inexact n)))) 0.0)])))

(define icosahedron-edge-length->circumsphere-radius : (-> Nonnegative-Flonum Nonnegative-Flonum)
  (lambda [a]
    (* a sin72º)))

(define icosahedron-radius->edge-length : (-> Nonnegative-Flonum 3D-Radius-Type Nonnegative-Flonum)
  (lambda [r type]
    (cond [(eq? type 'edge)   (* r 2.0 1/phi)] ; midsphere    R = a•φ/2
          [(eq? type 'face)   (* r 2√3 1/φ²)]  ; insphere     R = a•φ²/(2√3)
          [(eq? type 'vertex) (* r 1/sin72º)]  ; circumsphere R = a•sin72º = a•√(φ²+1)/2
          [else '#:deadcode r])))

(define icosahedron-radius->circumsphere-radius : (-> Nonnegative-Flonum 3D-Radius-Type Nonnegative-Flonum)
  (lambda [r type]
    (cond [(eq? type 'vertex) r]
          [else (icosahedron-edge-length->circumsphere-radius
                 (icosahedron-radius->edge-length r type))])))


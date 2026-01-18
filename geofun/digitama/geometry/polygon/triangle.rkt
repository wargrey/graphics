#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Triangle-Vertices (List Float-Complex Float-Complex Float-Complex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-isosceles-triangle-vertices/apex@top : (-> Nonnegative-Flonum Nonnegative-Flonum Triangle-Vertices)
  (lambda [width height]
    (list (make-rectangular (* width 0.5) 0.0)
          (make-rectangular width height) (make-rectangular 0.0 height))))

(define geo-isosceles-triangle-vertices/apex@bot : (-> Nonnegative-Flonum Nonnegative-Flonum Triangle-Vertices)
  (lambda [width height]
    (list 0.0+0.0i (make-rectangular width 0.0)
          (make-rectangular (* width 0.5) height))))

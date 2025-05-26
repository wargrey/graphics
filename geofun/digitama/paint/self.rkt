#lang typed/racket/base

(provide (all-defined-out))

(require "../base.rkt")
(require "stroke.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://svgwg.org/svg2-draft/painting.html
(define-type CSS-Gradient-Stop-Color (Pairof Real FlRGBA))
(define-type CSS-Linear-Gradient (Vector Real Real Real Real (Listof CSS-Gradient-Stop-Color)))
(define-type CSS-Radial-Gradient (Vector Real Real Real Real Real Real (Listof CSS-Gradient-Stop-Color)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Don't forget unsafe/paint.rkt if changing the Stroke
;;; No need to cache the instances of Stroke
;;    as it has already implemented as a struct rather than a class 
(struct stroke
  ([color : FlRGBA]
   [width : Nonnegative-Flonum]
   [linecap : Stroke-Cap-Style]
   [linejoin : Stroke-Join-Style]
   [miterlimit : Flonum]
   [dash : (Immutable-Vectorof Nonnegative-Flonum)]
   [offset : Flonum]
   [opacity : Flonum])
  #:type-name Stroke
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (T) stroke-maybe-width : (->* (Any) (T) (U Nonnegative-Flonum T))
  (lambda [s [fallback-width 0.0]]
    (if (stroke? s) (stroke-width s) fallback-width)))

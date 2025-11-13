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
;;; Don't forget unsafe/paint.rkt if changing the Pen
;;; No need to cache the instances of Pen
;;    as it has already implemented as a struct rather than a class 
(struct pen
  ([color : FlRGBA]
   [width : Nonnegative-Flonum]
   [linecap : Stroke-Cap-Style]
   [linejoin : Stroke-Join-Style]
   [miterlimit : Flonum]
   [dash : (Immutable-Vectorof Nonnegative-Flonum)]
   [offset : Flonum]
   [opacity : Flonum])
  #:type-name Pen
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (T) pen-maybe-width : (->* (Any) (T) (U Nonnegative-Flonum T))
  (lambda [s [fallback-width 0.0]]
    (if (pen? s) (pen-width s) fallback-width)))

(define #:forall (T) pen-adjust-color : (-> Pen (-> FlRGBA FlRGBA) Pen)
  (lambda [s adjust]
    (define oc (pen-color s))
    (define ac (adjust oc))

    (cond [(equal? ac oc) s]
          [else (pen ac (pen-width s)
                     (pen-linecap s) (pen-linejoin s) (pen-miterlimit s)
                     (pen-dash s) (pen-offset s)
                     (pen-opacity s))])))

#lang typed/racket/base

(provide (all-defined-out))

(require "../base.rkt")
(require "stroke.rkt")

(require "../unsafe/typed/c.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://svgwg.org/svg2-draft/painting.html
(define-type CSS-Gradient-Stop-Color (Pairof Real FlRGBA))
(define-type CSS-Linear-Gradient (Vector Real Real Real Real (Listof CSS-Gradient-Stop-Color)))
(define-type CSS-Radial-Gradient (Vector Real Real Real Real Real Real (Listof CSS-Gradient-Stop-Color)))

(define-type Fill-Rule (U 'winding 'even-odd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Don't forget unsafe/paint.rkt whenever the Pen or Brush are changed
;;; No need to cache the instances of Pen or Brush
;;    as they've already been implemented as structs rather than classes 
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

(struct brush
  ([color : FlRGBA]
   [pattern : (Option Cairo-Surface)]
   [opacity : Flonum]
   [rule : Fill-Rule])
  #:type-name Brush
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (T) pen-maybe-width : (->* (Any) (T) (U Nonnegative-Flonum T))
  (lambda [self [fallback-width 0.0]]
    (if (pen? self) (pen-width self) fallback-width)))

(define #:forall (T) pen-adjust-color : (-> Pen (-> FlRGBA FlRGBA) Pen)
  (lambda [self adjust]
    (define oc (pen-color self))
    (define ac (adjust oc))

    (cond [(equal? ac oc) self]
          [else (pen ac (pen-width self)
                     (pen-linecap self) (pen-linejoin self) (pen-miterlimit self)
                     (pen-dash self) (pen-offset self)
                     (pen-opacity self))])))

(define #:forall (T) brush-adjust-color : (-> Brush (-> FlRGBA FlRGBA) Brush)
  (lambda [self adjust]
    (define oc (brush-color self))
    (define ac (adjust oc))

    (cond [(equal? ac oc) self]
          [else (brush ac (brush-pattern self) (brush-opacity self) (brush-rule self))])))

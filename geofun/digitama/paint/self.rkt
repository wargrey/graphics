#lang typed/racket/base

(provide (all-defined-out))

(require "../base.rkt")
(require "../unsafe/typed/c.rkt")
(require "../unsafe/visual.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://svgwg.org/svg2-draft/painting.html
(define-type CSS-Gradient-Stop-Color (Pairof Real FlRGBA))
(define-type CSS-Linear-Gradient (Vector Real Real Real Real (Listof CSS-Gradient-Stop-Color)))
(define-type CSS-Radial-Gradient (Vector Real Real Real Real Real Real (Listof CSS-Gradient-Stop-Color)))

(define-type Stroke-Paint (U Color Pen))
(define-type Option-Stroke-Paint (Option Stroke-Paint))
(define-type Maybe-Stroke-Paint (U Option-Stroke-Paint Void))

(define-type Fill-Paint (U Color Visual-Object<%> Brush))
(define-type Option-Fill-Paint (Option Fill-Paint))
(define-type Maybe-Fill-Paint (U Option-Fill-Paint Void))
(define-type Fill-Rule (U 'winding 'even-odd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Don't forget unsafe/paint.rkt whenever the Pen or Brush are changed
;;; No need to cache the instances of Pen or Brush
;;    as they've already been implemented as structs rather than classes 
(struct pen
  ([color : FlRGBA]
   [width : Nonnegative-Flonum]
   [linecap : Byte]
   [linejoin : Byte]
   [miterlimit : Flonum]
   [dash : (Immutable-Vectorof Nonnegative-Flonum)]
   [offset : Flonum]
   [opacity : Flonum]
   [scalable? : Boolean])
  #:type-name Pen
  #:transparent)

(struct halo-pen
  ([colors : (Listof (Pairof FlRGBA Nonnegative-Flonum))]
   [width : Nonnegative-Flonum]
   [round? : Boolean]
   [opacity : Flonum]
   [scalable? : Boolean])
  #:type-name Halo-Pen
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
    (cond [(pen? self) (pen-width self)]
          [(halo-pen? self) (halo-pen-width self)]
          [else fallback-width])))

(define pen-select-thickest : (case-> [(Listof Any) -> (Option (U Pen Halo-Pen))]
                                      [(U Pen Halo-Pen) (Listof (U Pen Halo-Pen)) -> (U Pen Halo-Pen)])
  (case-lambda
    [(selves)
     (let ([pens (for/list : (Listof (U Pen Halo-Pen)) ([self (in-list selves)]
                                                        #:when (or (pen? self) (halo-pen? self)))
                   self)])
       (and (pair? pens)
            (pen-select-thickest (car pens) (cdr pens))))]
    [(self extras)
     (let select ([pen : (U Pen Halo-Pen) self]
                  [extras : (Listof (U Pen Halo-Pen)) extras])
       (cond [(null? extras) pen]
             [else (let-values ([(head tail) (values (car extras) (cdr extras))])
                     (cond [(>= (pen-maybe-width pen) (pen-maybe-width head)) (select pen tail)]
                           [else (select head tail)]))]))]))

(define #:forall (T) pen-adjust-color : (-> Pen (-> FlRGBA FlRGBA) Pen)
  (lambda [self adjust]
    (define oc (pen-color self))
    (define ac (adjust oc))

    (cond [(equal? ac oc) self]
          [else (pen ac (pen-width self)
                     (pen-linecap self) (pen-linejoin self) (pen-miterlimit self)
                     (pen-dash self) (pen-offset self)
                     (pen-opacity self)
                     (pen-scalable? self))])))

(define #:forall (T) brush-adjust-color : (-> Brush (-> FlRGBA FlRGBA) Brush)
  (lambda [self adjust]
    (define oc (brush-color self))
    (define ac (adjust oc))

    (cond [(equal? ac oc) self]
          [else (brush ac (brush-pattern self) (brush-opacity self) (brush-rule self))])))

#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [geo-intrinsic-width  geo-width]
                     [geo-intrinsic-height geo-height]
                     [geo-intrinsic-size   geo-size]
                     [geo-intrinsic-size   geo-intrinsic-flsize]
                     [geo<%>-outline       geo-outline]))

(require "paint/self.rkt")
(require "geometry/ink.rkt")

(require "unsafe/visual.rkt")
(require "unsafe/typed/cairo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-geometry-density : (Parameterof Positive-Flonum) (make-parameter 1.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Surface-Draw! (Cairo-Surface-Draw! Geo<%>))
(define-type Geo-Calculate-Extent (-> Geo<%> (Values Nonnegative-Flonum Nonnegative-Flonum (Option Geo-Ink))))
(define-type Geo-Calculate-Extent* (-> Geo<%> (Values Nonnegative-Flonum Nonnegative-Flonum Geo-Ink)))
(define-type Geo-Calculate-Outline (-> Geo<%> Option-Stroke-Paint Option-Stroke-Paint Geo-Pad))
(define-type Geo-Outline-Datum (U False Geo-Pad Geo-Calculate-Outline))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo<%> visual-object<%>
  ([draw! : Geo-Surface-Draw!]
   [extent : Geo-Calculate-Extent]
   [outline : Geo-Outline-Datum])
  #:type-name Geo<%>)

(struct geo geo<%>
  ([id : Symbol])
  #:type-name Geo
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-extent : Geo-Calculate-Extent
  (lambda [self]
    ((geo<%>-extent self) self)))

(define geo-intrinsic-size : (-> Geo<%> (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [self]
    (define-values (w h _) (geo-extent self))
    (values w h)))

(define geo-intrinsic-width : (-> Geo<%> Nonnegative-Flonum)
  (lambda [self]
    (define-values (w _h _) (geo-extent self))
    w))

(define geo-intrinsic-height : (-> Geo<%> Nonnegative-Flonum)
  (lambda [self]
    (define-values (_w h _) (geo-extent self))
    h))

(define geo-flsize : (case-> [Geo<%> -> (Values Nonnegative-Flonum Nonnegative-Flonum)]
                             [Geo<%> Nonnegative-Real -> (Values Nonnegative-Flonum Nonnegative-Flonum)]
                             [Geo<%> Nonnegative-Real Nonnegative-Real -> (Values Nonnegative-Flonum Nonnegative-Flonum)])
  (let ([flsize (λ [[geo : Geo<%>] [w% : Nonnegative-Flonum] [h% : Nonnegative-Flonum]] : (Values Nonnegative-Flonum Nonnegative-Flonum)
                  (let-values ([(w h) (geo-flsize geo)])
                    (values (* w w%) (* h h%))))])
    (case-lambda
      [(self w% h%) (flsize self (real->double-flonum w%) (real->double-flonum h%))]
      [(self ratio) (let ([% (real->double-flonum ratio)]) (flsize self % %))]
      [(self) (geo-intrinsic-size self)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; used in kernel only
(struct geo-pad
  ([top : Nonnegative-Flonum]
   [right : Nonnegative-Flonum]
   [bottom : Nonnegative-Flonum]
   [left : Nonnegative-Flonum])
  #:type-name Geo-Pad
  #:transparent)

(define geo-zero-pads : Geo-Pad (geo-pad 0.0 0.0 0.0 0.0))
(define geo-pad-scale : (case-> [Geo-Pad Flonum Flonum -> Geo-Pad]
                                [Geo-Outline-Datum Geo Flonum Flonum -> Geo-Outline-Datum])
  (case-lambda
    [(self sx0 sy0)
     (cond [(eq? self geo-zero-pads) geo-zero-pads]
           [else (let-values ([(sx sy) (values (abs sx0) (abs sy0))])
                   (geo-pad (* (geo-pad-top self)    sy)
                            (* (geo-pad-right self)  sx)
                            (* (geo-pad-bottom self) sy)
                            (* (geo-pad-left self)   sx)))])]
    [(self target sx0 sy0)
     (cond [(geo-pad? self) (geo-pad-scale self sx0 sy0)]
           [else (and self
                      (λ [[master : Geo<%>] [stroke : Option-Stroke-Paint] [border : Option-Stroke-Paint]] : Geo-Pad
                        (geo-pad-scale (self target stroke border) sx0 sy0)))])]))

(define geo-pad-expand : (-> Geo-Pad Nonnegative-Flonum Nonnegative-Flonum (Values Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [self width height]
    (define toff (geo-pad-top self))
    (define loff (geo-pad-left self))
    
    (values loff toff
            (+ width loff (geo-pad-right self))
            (+ height toff (geo-pad-bottom self)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(define geo-shape-extent : (case-> [Nonnegative-Flonum -> Geo-Calculate-Extent]
                                   [Nonnegative-Flonum Nonnegative-Flonum -> Geo-Calculate-Extent]
                                   [Nonnegative-Flonum Flonum Flonum -> Geo-Calculate-Extent*]
                                   [Nonnegative-Flonum Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum -> Geo-Calculate-Extent*]
                                   [Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum -> Geo-Calculate-Extent*]
                                   [Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum -> Geo-Calculate-Extent*])
  (case-lambda
    [(size) (λ [self] (values size size #false))]
    [(width height) (λ [self] (values width height #false))]
    [(size x y) (geo-shape-extent size size x y)]
    [(size x y w h) (geo-shape-extent size size x y w h)]
    [(width height x y) (λ [self] (values width height (make-geo-ink x y width height)))]
    [(width height x y w h) (λ [self] (values width height (make-geo-ink x y w h)))]))

(define geo-delegate-expand : (case-> [Geo -> Geo-Calculate-Extent]
                                      [Geo Flonum Flonum -> Geo-Calculate-Extent])
  (case-lambda
    [(self) (λ [who-cares] (geo-extent self))]
    [(self sx sy)
     (λ [who-cares]
       (let-values ([(owidth oheight ?oink) (geo-extent self)])
         (values (* (abs sx) owidth)
                 (* (abs sy) oheight)
                 (and ?oink (geo-ink-scale ?oink sx sy)))))]))

(define geo-shape-outline : (case-> [Maybe-Stroke-Paint -> (Option Geo-Pad)]
                                    [Maybe-Stroke-Paint Boolean Boolean -> (Option Geo-Pad)])
  (let ([insets : (HashTable Flonum Geo-Pad) (make-weak-hasheq)])
    (case-lambda
      [(stroke)
       (cond [(void? stroke) #false]
             [(not stroke) geo-zero-pads]
             [(pen? stroke) (geo-pen->outline stroke)]
             [else #false])]
      [(stroke x? y?)
       (cond [(void? stroke) #false]
             [(not stroke) geo-zero-pads]
             [(not (pen? stroke)) #false]
             [(and x? y?) (geo-pen->outline stroke)]
             [(or x? y?)
              (let* ([thickness (pen-width stroke)]
                     [offset (* thickness 0.5)]
                     [hoff (if (and x?) offset 0.0)]
                     [voff (if (and y?) offset 0.0)])
                (geo-pad voff hoff voff hoff))]
             [else geo-zero-pads])])))

(define geo-pen->outline : (-> Pen Geo-Pad)
  (let ([insets : (HashTable Flonum Geo-Pad) (make-weak-hasheq)])
    (lambda [stroke]
      (define thickness (pen-width stroke))

      (hash-ref! insets thickness
                 (λ [] (let ([offset (* thickness 0.5)])
                         (geo-pad offset offset offset offset)))))))

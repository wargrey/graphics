#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [geo-intrinsic-width  geo-width]
                     [geo-intrinsic-height geo-height]
                     [geo-intrinsic-size   geo-size]
                     [geo-intrinsic-size   geo-intrinsic-flsize]))

(require racket/symbol)

(require "paint/self.rkt")
(require "geometry/ink.rkt")
(require "geometry/bleed.rkt")

(require "unsafe/visual.rkt")
(require "unsafe/typed/cairo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Surface-Draw! (Cairo-Surface-Draw! Geo<%>))
(define-type Geo-Calculate-Extent (-> Geo<%> (Values Nonnegative-Flonum Nonnegative-Flonum (Option Geo-Ink))))
(define-type Geo-Calculate-Extent* (-> Geo<%> (Values Nonnegative-Flonum Nonnegative-Flonum Geo-Ink)))
(define-type Geo-Calculate-Bleed (-> Geo<%> Option-Stroke-Paint Option-Stroke-Paint Geo-Bleed))
(define-type Geo-Bleed-Datum (U False Geo-Bleed Geo-Calculate-Bleed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo<%> visual-object<%>
  ([draw! : Geo-Surface-Draw!]
   [extent : Geo-Calculate-Extent]
   [bleed : Geo-Bleed-Datum])
  #:type-name Geo<%>)

(struct geo geo<%>
  ([id : Symbol]
   [desc : (Option String)])
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

(define geo-aspect-ratio : (-> Geo<%> Nonnegative-Flonum)
  (lambda [self]
    (define-values (w h _) (geo-extent self))

    (max 0.0 (/ w h))))

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

(define geo->string : (-> Geo String)
  (lambda [self]
    (or (geo-desc self)
        (symbol->immutable-string (geo-id self)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-bleed-scale* : (case-> [Geo-Bleed Flonum Flonum -> Geo-Bleed]
                                   [Geo-Bleed-Datum Geo Flonum Flonum -> Geo-Bleed-Datum])
  (case-lambda
    [(self sx0 sy0) (geo-bleed-scale self sx0 sy0)]
    [(self target sx0 sy0)
     (cond [(geo-bleed? self) (geo-bleed-scale self sx0 sy0)]
           [else (and self
                      (λ [[master : Geo<%>] [stroke : Option-Stroke-Paint] [border : Option-Stroke-Paint]] : Geo-Bleed
                        (geo-bleed-scale (self target stroke border) sx0 sy0)))])]))

(define geo-shape-bleed : (case-> [(U Maybe-Stroke-Paint Halo-Pen) -> (Option Geo-Bleed)]
                                  [Option-Stroke-Paint Option-Stroke-Paint -> (Option Geo-Bleed)]
                                  [Maybe-Stroke-Paint Boolean Boolean -> (Option Geo-Bleed)])
  (case-lambda
    [(stroke)
     (cond [(void? stroke) #false]
           [(not stroke) geo-zero-bleeds]
           [(pen? stroke) (geo-pen->bleed stroke)]
           [(halo-pen? stroke) (geo-pen->bleed stroke)]
           [else #false])]
    [(stroke border)
     (geo-shape-bleed (or border stroke))]
    [(stroke x? y?)
     (cond [(void? stroke) #false]
           [(not stroke) geo-zero-bleeds]
           [(not (pen? stroke)) #false]
           [(and x? y?) (geo-pen->bleed stroke)]
           [(or x? y?)
            (let* ([thickness (pen-width stroke)]
                   [scalable? (pen-scalable? stroke)]
                   [offset (* thickness 0.5)]
                   [hoff (if (and x?) offset 0.0)]
                   [voff (if (and y?) offset 0.0)])
              (geo-bleed voff scalable? hoff scalable? voff scalable? hoff scalable?))]
           [else geo-zero-bleeds])]))

(define geo-pen->bleed : (-> (U Pen Halo-Pen) Geo-Bleed)
  (let ([sbleeds : (HashTable Flonum Geo-Bleed) (make-weak-hasheq)]
        [fbleeds : (HashTable Flonum Geo-Bleed) (make-weak-hasheq)])
    (lambda [stroke]
      (define-values (thickness scalable?)
        (if (pen? stroke)
            (values (pen-width stroke) (pen-scalable? stroke))
            (values (halo-pen-width stroke) (halo-pen-scalable? stroke))))

      (hash-ref! (if scalable? sbleeds fbleeds) thickness
                 (λ [] (let ([offset (* thickness 0.5)])
                         (geo-bleed offset scalable? offset scalable?
                                    offset scalable? offset scalable?)))))))

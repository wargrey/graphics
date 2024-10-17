#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [geo-intrinsic-width  geo-width]
                     [geo-intrinsic-height geo-height]
                     [geo-intrinsic-size   geo-size]
                     [geo-intrinsic-size   geo-intrinsic-flsize]))

(require "source.rkt")
(require "dc/ink.rkt")
(require "dc/paint.rkt")
(require "../paint.rkt")
(require "../stroke.rkt")

(require "unsafe/visual/ctype.rkt")
(require "unsafe/visual/object.rkt")
(require "unsafe/visual/abstract.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

(require typed/racket/unsafe)
(unsafe-require/typed
 "unsafe/surface/abstract.rkt"
 [cairo-create-abstract-surface* (-> Flonum Flonum Positive-Flonum Boolean
                                     (Values Abstract-Surface Cairo-Ctx Positive-Index Positive-Index))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (create-geometry-object stx)
  (syntax-parse stx #:datum-literals [:]
    [(_ Geo
        (~seq #:surface surface:expr wrapper:expr ...)
        (~optional (~seq #:extent extent:expr) #:defaults ([extent #'geo-calculate-extent]))
        (~optional (~seq #:id name) #:defaults ([name #'#false])) argl ...)
     (with-syntax ([geo-prefix (datum->syntax #'Geo (format "~a:" (syntax->datum #'Geo)))])
       (syntax/loc stx
         (Geo geo-convert (geo-shape-surface-wrapper surface wrapper ...) extent
              (or name (gensym 'geo-prefix)) argl ...)))]))

(define default-geometry-density : (Parameterof Positive-Flonum) (make-parameter 1.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Surface-Draw! (-> Geo<%> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Void))
(define-type Geo-Calculate-Extent (-> Geo<%> (Values Nonnegative-Flonum Nonnegative-Flonum (Option Geo-Ink))))

(struct geo<%> visual-object<%>
  ([draw! : Geo-Surface-Draw!]
   [extent : Geo-Calculate-Extent])
  #:type-name Geo<%>)

(struct geo geo<%>
  ([id : Symbol])
  #:type-name Geo
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-extent : (->* (Geo<%>)
                          (Maybe-Stroke-Paint #:border Maybe-Stroke-Paint)
                          (Values Nonnegative-Flonum Nonnegative-Flonum (Option Geo-Ink)))
  (lambda [geo [stroke (void)] #:border [border (void)]]
    (parameterize ([default-stroke-source (geo-select-stroke-paint stroke)]
                   [default-border-source (geo-select-border-paint border)])
      ((geo<%>-extent geo) geo))))

(define geo-intrinsic-size : (->* (Geo<%>)
                                  (Maybe-Stroke-Paint #:border Maybe-Stroke-Paint)
                                  (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [geo [stroke (void)] #:border [border (void)]]
    (define-values (width height _) (geo-extent geo stroke #:border border))
    (values width height)))

(define geo-intrinsic-width : (-> Geo<%> Nonnegative-Flonum)
  (lambda [geo]
    (define-values (w h) (geo-intrinsic-size geo))
    w))

(define geo-intrinsic-height : (-> Geo<%> Nonnegative-Flonum)
  (lambda [geo]
    (define-values (w h) (geo-intrinsic-size geo))
    h))

(define geo-flsize : (case-> [Geo<%> -> (Values Nonnegative-Flonum Nonnegative-Flonum)]
                             [Geo<%> Nonnegative-Real -> (Values Nonnegative-Flonum Nonnegative-Flonum)]
                             [Geo<%> Nonnegative-Real Nonnegative-Real -> (Values Nonnegative-Flonum Nonnegative-Flonum)])
  (let ([flsize (λ [[geo : Geo<%>] [w% : Nonnegative-Flonum] [h% : Nonnegative-Flonum]] : (Values Nonnegative-Flonum Nonnegative-Flonum)
                  (let-values ([(w h) (geo-flsize geo)])
                    (values (* w w%) (* h h%))))])
    (case-lambda
      [(geo w% h%) (flsize geo (real->double-flonum w%) (real->double-flonum h%))]
      [(geo ratio) (let ([% (real->double-flonum ratio)]) (flsize geo % %))]
      [(geo) (geo-intrinsic-size geo)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-convert : Visual-Object-Convert
  (lambda [self mime fallback]
    (with-asserts ([self geo<%>?])
      (case mime
        ;[(pdf-bytes)    (abstract-surface->stream-bytes ((geo<%>-draw! self) self) 'pdf '/dev/pdfout 1.0)]
        ;[(svg-bytes)    (abstract-surface->stream-bytes ((geo<%>-draw! self) self) 'svg '/dev/svgout 1.0)]
        ;[(png@2x-bytes) (abstract-surface->stream-bytes ((geo<%>-draw! self) self) 'png '/dev/p2xout 2.0)]
        ;[(png-bytes)    (abstract-surface->stream-bytes ((geo<%>-draw! self) self) 'png '/dev/pngout 1.0)]
        [(cairo-surface) (geo-create-surface self)]
        [else fallback]))))

(define geo-create-surface : (->* (Geo<%>) (Positive-Flonum #:scale? Boolean) Abstract-Surface)
  (lambda [self [density (default-geometry-density)] #:scale? [scale? #true]]
    (define-values (flwidth flheight ink) ((geo<%>-extent self) self))
    (define-values (surface cr width height) (cairo-create-abstract-surface* flwidth flheight density scale?))

    ((geo<%>-draw! self) self cr 0.0 0.0 flwidth flheight)
    surface))

(define geo-calculate-extent : Geo-Calculate-Extent
  (lambda [self]
    (define sfc (geo-create-surface self))
    (define-values (?pos self-width self-height) (abstract-surface-extent sfc))

    (if (not ?pos)
        (let-values ([(pos ink-width ink-height) (abstract-surface-bbox sfc)])
          (values ink-width ink-height (make-geo-ink pos ink-width ink-height)))
        (values self-width self-height #false))))

(define geo-calculate-extent* : Geo-Calculate-Extent
  (lambda [self]
    (define sfc (geo-create-surface self))
    (define-values (?pos sfc-width sfc-height) (abstract-surface-extent sfc))
    (define-values (ink-pos ink-width ink-height) (abstract-surface-bbox sfc))

    (if (not ?pos)
        (values ink-width ink-height (make-geo-ink ink-pos ink-width ink-height))
        (values sfc-width sfc-height (make-geo-ink ink-pos ink-width ink-height)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-shape-plain-extent : (case-> [Nonnegative-Flonum -> Geo-Calculate-Extent]
                                         [Nonnegative-Flonum Flonum Flonum -> Geo-Calculate-Extent]
                                         [Nonnegative-Flonum Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum -> Geo-Calculate-Extent]
                                         [Nonnegative-Flonum Nonnegative-Flonum -> Geo-Calculate-Extent]
                                         [Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum -> Geo-Calculate-Extent]
                                         [Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum -> Geo-Calculate-Extent])
  (case-lambda
    [(size) (λ [self] (values size size #false))]
    [(width height) (λ [self] (values width height #false))]
    [(size x y) (geo-shape-plain-extent size size x y)]
    [(size x y w h) (geo-shape-plain-extent size size x y w h)]
    [(width height x y) (λ [self] (values width height (make-geo-ink x y width height)))]
    [(width height x y w h) (λ [self] (values width height (make-geo-ink x y w h)))]))

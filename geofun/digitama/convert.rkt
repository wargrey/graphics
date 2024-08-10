#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [geo-intrinsic-width geo-width]
                     [geo-intrinsic-height geo-height]
                     [geo-intrinsic-size geo-size]
                     [geo-intrinsic-size geo-intrinsic-flsize]))

(require "source.rkt")

(require "unsafe/visual/ctype.rkt")
(require "unsafe/visual/object.rkt")
(require "unsafe/visual/abstract.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

(require typed/racket/unsafe)
(unsafe-require/typed
 "unsafe/surface/abstract.rkt"
 [cairo-create-abstract-surface* (-> Flonum Flonum Positive-Flonum Boolean
                                     (Values Abstract-Surface Cairo-DC Positive-Index Positive-Index))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (create-geometry-object stx)
  (syntax-parse stx #:datum-literals [:]
    [(_ Geo
        (~seq #:with [surface:expr (~optional bbox:expr #:defaults ([bbox #'geo-calculate-bbox]))])
        (~optional (~seq #:id name) #:defaults ([name #'#false])) argl ...)
     (with-syntax ([geo-prefix (datum->syntax #'Geo (format "~a:" (syntax->datum #'Geo)))])
       (syntax/loc stx
         (Geo geo-convert surface bbox (or name (gensym 'geo-prefix)) argl ...)))]
    [(_ Geo (~seq #:with surface:id) rest ...)
     (syntax/loc stx
       (create-geometry-object Geo #:with [surface] rest ...))]))

(define default-geometry-density : (Parameterof Positive-Flonum) (make-parameter 1.0))

(define create-abstract-surface : (Cairo-Surface-Create Abstract-Surface)
  (lambda [flwidth flheight density scale?]
    (define-values (surface cr width height) (cairo-create-abstract-surface* flwidth flheight density scale?))
    (values surface cr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Surface-Create (-> Geo<%> Abstract-Surface))
(define-type Geo-Calculate-BBox (-> Geo<%> (Values Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum)))

(struct geo<%> visual-object<%>
  ([surface : Geo-Surface-Create]
   [aabox : Geo-Calculate-BBox])
  #:type-name Geo<%>)

(struct geo geo<%>
  ([id : Symbol])
  #:type-name Geo
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-bounding-box : (->* (Geo<%>) (Stroke-Paint) (Values Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [geo [stroke (default-border)]]
    (parameterize ([default-border (stroke-paint->source stroke)])
      ((geo<%>-aabox geo) geo))))

(define geo-intrinsic-size : (->* (Geo<%>) (Stroke-Paint) (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [geo [stroke (default-border)]]
    (define-values (lx ty width height) (geo-bounding-box geo stroke))
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
        [(pdf-bytes)    (abstract-surface->stream-bytes ((geo<%>-surface self) self) 'pdf '/dev/pdfout 1.0)]
        [(svg-bytes)    (abstract-surface->stream-bytes ((geo<%>-surface self) self) 'svg '/dev/svgout 1.0)]
        [(png@2x-bytes) (abstract-surface->stream-bytes ((geo<%>-surface self) self) 'png '/dev/p2xout 2.0)]
        [(png-bytes)    (abstract-surface->stream-bytes ((geo<%>-surface self) self) 'png '/dev/pngout 1.0)]
        [(cairo-surface) ((geo<%>-surface self) self)]
        [else fallback]))))

(define geo-calculate-bbox : Geo-Calculate-BBox
  (lambda [self]
    (abstract-surface-content-bbox
     ((geo<%>-surface self) self))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-shape-surface-wrapper : (case-> [Geo-Surface-Create (Option Stroke) (Option Stroke) (Option Fill-Paint) (Option Symbol) -> Geo-Surface-Create]
                                            [Geo-Surface-Create (Option Stroke) (Option Fill-Paint) (Option Symbol) -> Geo-Surface-Create]
                                            [Geo-Surface-Create (Option Stroke) (Option Fill-Paint) -> Geo-Surface-Create]
                                            [Geo-Surface-Create (Option Stroke) -> Geo-Surface-Create])
  (case-lambda
    [(λsurface alt-stroke alt-border-stroke alt-fill alt-rule)
     (geo-shape-surface-wrapper (geo-shape-surface-wrapper λsurface alt-border-stroke alt-fill alt-rule) alt-stroke)]
    [(λsurface alt-border-stroke alt-fill alt-rule)
     (cond [(and alt-border-stroke alt-fill alt-rule)
            (λ [self]
              (parameterize ([default-border alt-border-stroke]
                             [default-fill-paint alt-fill]
                             [default-fill-rule alt-rule])
                (λsurface self)))]
           [(and alt-border-stroke alt-fill)
            (λ [self]
              (parameterize ([default-border alt-border-stroke]
                             [default-fill-paint alt-fill]
                             #;[default-fill-rule alt-rule])
                (λsurface self)))]
           [(and alt-fill alt-rule)
            (λ [self]
              (parameterize (#;[default-border alt-border-stroke]
                             [default-fill-paint alt-fill]
                             [default-fill-rule alt-rule])
                (λsurface self)))]
           [(and alt-border-stroke alt-rule)
            (λ [self]
              (parameterize ([default-border alt-border-stroke]
                             #;[default-fill-paint alt-fill]
                             [default-fill-rule alt-rule])
                (λsurface self)))]
           [(and alt-border-stroke)
            (λ [self]
              (parameterize ([default-border alt-border-stroke]
                             #;[default-fill-paint alt-fill]
                             #;[default-fill-rule alt-rule])
                (λsurface self)))]
           [(and alt-fill)
            (λ [self]
              (parameterize (#;[default-border alt-border-stroke]
                             [default-fill-paint alt-fill]
                             #;[default-fill-rule alt-rule])
                (λsurface self)))]
           [(and alt-rule)
            (λ [self]
              (parameterize (#;[default-border alt-border-stroke]
                             #;[default-fill-paint alt-fill]
                             [default-fill-rule alt-rule])
                (λsurface self)))]
           [else λsurface])]
    [(λsurface alt-border-stroke alt-fill)
     (cond [(and alt-border-stroke alt-fill)
            (λ [self]
              (parameterize ([default-border alt-border-stroke]
                             [default-fill-paint alt-fill])
                (λsurface self)))]
           [(and alt-fill)
            (λ [self]
              (parameterize (#;[default-border alt-border-stroke]
                             [default-fill-paint alt-fill])
                (λsurface self)))]
           [(and alt-border-stroke)
            (λ [self]
              (parameterize ([default-border alt-border-stroke]
                             #;[default-fill-paint alt-fill])
                (λsurface self)))]
           [else λsurface])]
    [(λsurface alt-stroke)
     (cond [(and alt-stroke)
            (λ [self]
              (parameterize ([default-stroke alt-stroke])
                (λsurface self)))]
           [else λsurface])]))


(define geo-shape-bbox-wrapper : (-> Geo-Calculate-BBox (Option Stroke)  Geo-Calculate-BBox)
  (lambda [λsurface alt-border-stroke]
    (if (or alt-border-stroke)
        (λ [self]
          (parameterize ([default-border alt-border-stroke])
            (λsurface self)))
        λsurface)))

(define geo-shape-plain-bbox : (case-> [Nonnegative-Flonum -> Geo-Calculate-BBox]
                                       [Nonnegative-Flonum Nonnegative-Flonum -> Geo-Calculate-BBox])
  (case-lambda
    [(size) (λ [self] (values 0.0 0.0 size size))]
    [(width height) (λ [self] (values 0.0 0.0 width height))]))

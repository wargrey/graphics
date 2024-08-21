#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [geo-intrinsic-width geo-width]
                     [geo-intrinsic-height geo-height]
                     [geo-intrinsic-size geo-size]
                     [geo-intrinsic-size geo-intrinsic-flsize]))

(require "dc/paint.rkt")
(require "source.rkt")
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

(define create-abstract-surface : (Cairo-Surface-Create Abstract-Surface)
  (lambda [flwidth flheight density scale?]
    (define-values (surface cr width height) (cairo-create-abstract-surface* flwidth flheight density scale?))
    (values surface cr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-bounding-box : (->* (Geo<%>) (Maybe-Stroke-Paint) (Values Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [geo [stroke (default-border-paint)]]
    (parameterize ([default-border-source (border-paint->source* stroke)])
      ((geo<%>-aabox geo) geo))))

(define geo-intrinsic-size : (->* (Geo<%>) (Maybe-Stroke-Paint) (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [geo [stroke (default-border-paint)]]
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
        [(cairo-surface) (geo-create-surface self)]
        [else fallback]))))

(define geo-create-surface : (-> Geo<%> Abstract-Surface)
  (lambda [self]
    ((geo<%>-surface self) self)))

(define geo-calculate-bbox : Geo-Calculate-BBox
  (lambda [self]
    (abstract-surface-content-bbox
     (geo-create-surface self))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-shape-surface-wrapper : (case-> [Geo-Surface-Create Maybe-Stroke-Paint Maybe-Stroke-Paint Maybe-Fill-Paint (Option Symbol) -> Geo-Surface-Create]
                                            [Geo-Surface-Create Maybe-Stroke-Paint Maybe-Fill-Paint (Option Symbol) -> Geo-Surface-Create]
                                            [Geo-Surface-Create Maybe-Stroke-Paint Maybe-Fill-Paint -> Geo-Surface-Create]
                                            [Geo-Surface-Create Maybe-Stroke-Paint -> Geo-Surface-Create])
  (case-lambda
    [(λsurface alt-stroke alt-border alt-fill alt-rule)
     (geo-shape-surface-wrapper (geo-shape-surface-wrapper λsurface alt-border alt-fill alt-rule) alt-stroke)]
    [(λsurface alt-border alt-fill alt-rule)
     (define-values (border-set? fill-set?)
       (values (not (void? alt-border))
               (not (void? alt-fill))))
     
     (cond [(and border-set? fill-set? alt-rule)
            (λ [self]
              (parameterize ([default-border-source (border-paint->source* alt-border)]
                             [default-fill-source (fill-paint->source* alt-fill)]
                             [default-fill-rule alt-rule])
                (λsurface self)))]
           [(and border-set? fill-set?)
            (λ [self]
              (parameterize ([default-border-source (border-paint->source* alt-border)]
                             [default-fill-source (fill-paint->source* alt-fill)]
                             #;[default-fill-rule alt-rule])
                (λsurface self)))]
           [(and fill-set? alt-rule)
            (λ [self]
              (parameterize (#;[default-border-source (border-paint->source* alt-border)]
                             [default-fill-source (fill-paint->source* alt-fill)]
                             [default-fill-rule alt-rule])
                (λsurface self)))]
           [(and border-set? alt-rule)
            (λ [self]
              (parameterize ([default-border-source (border-paint->source* alt-border)]
                             #;[default-fill-source (fill-paint->source* alt-fill)]
                             [default-fill-rule alt-rule])
                (λsurface self)))]
           [(and border-set?)
            (λ [self]
              (parameterize ([default-border-source (border-paint->source* alt-border)]
                             #;[default-fill-source (fill-paint->source* alt-fill)]
                             #;[default-fill-rule alt-rule])
                (λsurface self)))]
           [(and fill-set?)
            (λ [self]
              (parameterize (#;[default-border-source (border-paint->source* alt-border)]
                             [default-fill-source (fill-paint->source* alt-fill)]
                             #;[default-fill-rule alt-rule])
                (λsurface self)))]
           [(and alt-rule)
            (λ [self]
              (parameterize (#;[default-border-source (border-paint->source* alt-border)]
                             #;[default-fill-source (fill-paint->source* alt-fill)]
                             [default-fill-rule alt-rule])
                (λsurface self)))]
           [else λsurface])]
    [(λsurface alt-border alt-fill)
     (define-values (border-set? fill-set?)
       (values (not (void? alt-border))
               (not (void? alt-fill))))
     
     (cond [(and border-set? fill-set?)
            (λ [self]
              (parameterize ([default-border-source (border-paint->source* alt-border)]
                             [default-fill-source (fill-paint->source* alt-fill)])
                (λsurface self)))]
           [(and fill-set?)
            (λ [self]
              (parameterize (#;[default-border-source (border-paint->source* alt-border)]
                             [default-fill-source (fill-paint->source* alt-fill)])
                (λsurface self)))]
           [(and border-set?)
            (λ [self]
              (parameterize ([default-border-source (border-paint->source* alt-border)]
                             #;[default-fill-source (fill-paint->source* alt-fill)])
                (λsurface self)))]
           [else λsurface])]
    [(λsurface alt-stroke)
     (cond [(void? alt-stroke) λsurface]
           [else (λ [self] (parameterize ([default-stroke-source (stroke-paint->source* alt-stroke)])
                             (λsurface self)))])]))

(define geo-shape-bbox-wrapper : (-> Geo-Calculate-BBox Maybe-Stroke-Paint Geo-Calculate-BBox)
  (lambda [λbbox alt-border]
    (cond [(void? alt-border) λbbox]
          [else (λ [self] (parameterize ([default-border-source (border-paint->source* alt-border)])
                            (λbbox self)))])))

(define geo-path-bbox-wrapper : (-> Geo-Calculate-BBox Geo-Calculate-BBox)
  (lambda [λbbox]
    (λ [self]
      (let-values ([(x y width height) (λbbox self)])
        (define maybe-stroke (current-stroke-source))
        (if (stroke? maybe-stroke)
            (let ([linewidth (stroke-width maybe-stroke)])
              (values x y (+ width linewidth) (+ height linewidth)))
            (values x y width height))))))

(define geo-shape-plain-bbox : (case-> [(U Nonnegative-Flonum Geo<%>) -> Geo-Calculate-BBox]
                                       [Nonnegative-Flonum Nonnegative-Flonum -> Geo-Calculate-BBox]
                                       [Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum -> Geo-Calculate-BBox])
  (case-lambda
    [(size)
     (if (flonum? size)
         (λ [self] (values 0.0 0.0 size size))
         (geo<%>-aabox size))]
    [(width height) (λ [self] (values 0.0 0.0 width height))]
    [(x y width height) (λ [self] (values x y width height))]))

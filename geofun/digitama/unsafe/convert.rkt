#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [geo-intrinsic-width geo-width]
                     [geo-intrinsic-height geo-height]
                     [geo-intrinsic-size geo-size]
                     [geo-intrinsic-size geo-intrinsic-flsize]))

(require bitmap/digitama/unsafe/visual/ctype)
(require bitmap/digitama/unsafe/visual/object)
(require bitmap/digitama/unsafe/visual/abstract)

(require bitmap/digitama/source)

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (create-geometry-object stx)
  (syntax-parse stx #:datum-literals [:]
    [(_ Geo
        (~seq #:with [surface:id (~optional bbox:id #:defaults ([bbox #'geo-calculate-bbox]))])
        (~optional (~seq #:id name) #:defaults ([name #'#false])) argl ...)
     (syntax/loc stx
       (Geo geo-convert surface bbox (or name (gensym 'geo)) argl ...))]
    [(_ Geo (~seq #:with surface:id) rest ...)
     (syntax/loc stx
       (create-geometry-object Geo #:with [surface] rest ...))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Unique-Identifier (U Keyword Symbol))
(define-type Geo-Surface-Create (->* (Geo<%>) (Stroke-Paint (Option Fill-Paint) Symbol) Abstract-Surface))
(define-type Geo-Calculate-BBox (->* (Geo<%>) (Stroke-Paint) (Values Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum)))

(struct geo<%> visual-object<%>
  ([surface : Geo-Surface-Create]
   [aabox : Geo-Calculate-BBox])
  #:type-name Geo<%>)

(struct geo geo<%>
  ([id : Geo-Unique-Identifier])
  #:type-name Geo
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-bounding-box : (->* (Geo<%>) (Stroke-Paint) (Values Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [geo [stroke (default-stroke)]]
    ((geo<%>-aabox geo) geo stroke)))

(define geo-intrinsic-size : (->* (Geo<%>) (Stroke-Paint) (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [geo [stroke (default-stroke)]]
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
        [else fallback]))))

(define geo-calculate-bbox : Geo-Calculate-BBox
  (lambda [self [stroke (default-stroke)]]
    (abstract-surface-content-bbox ((geo<%>-surface self) self stroke #false))))

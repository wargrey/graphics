#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [geo-intrinsic-width  geo-width]
                     [geo-intrinsic-height geo-height]
                     [geo-intrinsic-size   geo-size]
                     [geo-intrinsic-size   geo-intrinsic-flsize]))

(require "../stroke.rkt")

(require "paint.rkt")
(require "geometry/ink.rkt")

(require "unsafe/visual.rkt")
(require "unsafe/typed/cairo.rkt")
(require "unsafe/surface/abstract.rkt")
(require "unsafe/surface/image.rkt")

(require "unsafe/stream/vector.rkt")
(require "unsafe/stream/pdf.rkt")
(require "unsafe/stream/svg.rkt")
(require "unsafe/stream/png.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (create-geometry-object stx)
  (syntax-parse stx #:datum-literals [:]
    [(_ Geo #:with [name draw!:expr extent:expr] argl ...)
     (with-syntax ([geo-prefix (datum->syntax #'Geo (format "~a:" (syntax->datum #'Geo)))])
       (syntax/loc stx
         (Geo geo-convert draw! extent
              (or name (gensym 'geo-prefix)) argl ...)))]))

(define default-geometry-density : (Parameterof Positive-Flonum) (make-parameter 1.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Surface-Draw! (Cairo-Surface-Draw! Geo<%>))
(define-type Geo-Calculate-Extent (-> Geo<%> (Values Nonnegative-Flonum Nonnegative-Flonum (Option Geo-Ink))))
(define-type Geo-Calculate-Extent* (-> Geo<%> (Values Nonnegative-Flonum Nonnegative-Flonum Geo-Ink)))

(struct geo<%> visual-object<%>
  ([draw! : Geo-Surface-Draw!]
   [extent : Geo-Calculate-Extent])
  #:type-name Geo<%>)

(struct geo geo<%>
  ([id : Symbol])
  #:type-name Geo
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-extent : (-> Geo<%> (Values Nonnegative-Flonum Nonnegative-Flonum (Option Geo-Ink)))
  (lambda [geo]
    ((geo<%>-extent geo) geo)))

(define geo-extent* : Geo-Calculate-Extent*
  (lambda [self]
    (define-values (width height ink) (geo-extent self))
    
    (if (not ink)
        (let*-values ([(sfc) (geo-object->surface self 1.0 cairo-create-abstract-surface*)]
                      [(ink-x ink-y ink-width ink-height) (abstract-surface-bbox sfc)])
          (values width height (make-geo-ink ink-x ink-y ink-width ink-height)))
        (values width height ink))))

(define geo-intrinsic-size : (-> Geo<%> (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [geo]
    (define-values (width height _) (geo-extent geo))
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
        [(pdf-bytes)     (geo-object->stream-bytes self 'pdf 1.0 '/dev/pdfout)]
        [(svg-bytes)     (geo-object->stream-bytes self 'svg 1.0 '/dev/svgout)]
        [(png@2x-bytes)  (geo-object->stream-bytes self 'png 2.0 '/dev/p2xout)]
        [(png-bytes)     (geo-object->stream-bytes self 'png 1.0 '/dev/pngout)]
        [(cairo-surface) (geo-object->surface self 1.0 cairo-create-abstract-surface*)]
        [else fallback]))))

(define geo-object->stream-bytes : (->* (Geo<%> Symbol) (Positive-Flonum Symbol) Bytes)
  (lambda [self format [density 1.0] [name #false]]
    (define /dev/geoout : Output-Port (open-output-bytes name))
    (geo-object-save self /dev/geoout format density)
    (get-output-bytes /dev/geoout)))

(define geo-object-save : (-> Geo<%> (U Path-String Output-Port) Symbol Positive-Flonum Void)
  (lambda [self /dev/geoout format density]
    (case format
      [(svg) (geo-object-save-vector-with cairo-svg-stream-write self /dev/geoout density)]
      [(pdf) (geo-object-save-vector-with cairo-pdf-stream-write self /dev/geoout density)]
      [else  (cairo-png-stream-write /dev/geoout (λ [] (values (geo-object->surface self density cairo-create-argb-image-surface*) #true)))])))

(define geo-object-save-vector-with : (-> (Cairo-Vector-Stream-Write Geo<%>) Geo<%> (U Path-String Output-Port) Positive-Flonum Void)
  (lambda [stream-write self /dev/strout density]
    (define s : (Option Stroke) (current-stroke-source))
    (define thickness (stroke-maybe-width s))
    (define offset (* thickness 0.5))
    (define-values (width height _ink) ((geo<%>-extent self) self))
  
    (stream-write /dev/strout (+ width thickness) (+ height thickness)
                  (λ [[master : Geo<%>] [vec-cr : Cairo-Ctx]
                                        [x0 : Flonum] [y0 : Flonum]
                                        [flwidth : Nonnegative-Flonum] [flheight : Nonnegative-Flonum]] : Any
                    (unless (= density 1.0)
                      (define s (/ 1.0 density))
                      (cairo_scale vec-cr s s))

                    ((geo<%>-draw! self) self vec-cr x0 y0 flwidth flheight))
                  self offset offset width height)))

(define #:forall (Sfc S) geo-object->surface : (-> Geo<%> Positive-Flonum
                                                   (-> Nonnegative-Flonum Nonnegative-Flonum Positive-Flonum Boolean
                                                       (Values Sfc Cairo-Ctx S S))
                                                   Sfc)
  (lambda [self density create-surface]
    (define s : (Option Stroke) (current-stroke-source))
    (define thickness (stroke-maybe-width s))
    (define offset (* thickness 0.5))
    (define-values (flwidth flheight _ink) ((geo<%>-extent self) self))
    (define-values (sfc cr fxwidth fxheight) (create-surface (+ flwidth thickness) (+ flheight thickness) density #true))
    
    ((geo<%>-draw! self) self cr offset offset flwidth flheight)
    sfc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(define geo-shape-plain-extent : (case-> [Nonnegative-Flonum -> Geo-Calculate-Extent]
                                         [Nonnegative-Flonum Nonnegative-Flonum -> Geo-Calculate-Extent]
                                         [Nonnegative-Flonum Flonum Flonum -> Geo-Calculate-Extent*]
                                         [Nonnegative-Flonum Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum -> Geo-Calculate-Extent*]
                                         [Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum -> Geo-Calculate-Extent*]
                                         [Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum -> Geo-Calculate-Extent*])
  (case-lambda
    [(size) (λ [self] (values size size #false))]
    [(width height) (λ [self] (values width height #false))]
    [(size x y) (geo-shape-plain-extent size size x y)]
    [(size x y w h) (geo-shape-plain-extent size size x y w h)]
    [(width height x y) (λ [self] (values width height (make-geo-ink x y width height)))]
    [(width height x y w h) (λ [self] (values width height (make-geo-ink x y w h)))]))

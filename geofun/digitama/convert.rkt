#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [geo-intrinsic-width  geo-width]
                     [geo-intrinsic-height geo-height]
                     [geo-intrinsic-size   geo-size]
                     [geo-intrinsic-size   geo-intrinsic-flsize]
                     [geo<%>-outline       geo-outline]))

(require "../paint.rkt")

(require "paint.rkt")
(require "composite.rkt")
(require "paint/self.rkt")
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
    [(_ Geo #:with [name draw!:expr extent:expr insets:expr] argl ...)
     (with-syntax ([geo-prefix (datum->syntax #'Geo (format "~a:" (syntax->datum #'Geo)))])
       (syntax/loc stx
         (Geo geo-convert draw! extent insets
              (or name (gensym 'geo-prefix)) argl ...)))]))

(define default-geometry-density : (Parameterof Positive-Flonum) (make-parameter 1.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Surface-Draw! (Cairo-Surface-Draw! Geo<%>))
(define-type Geo-Calculate-Extent (-> Geo<%> (Values Nonnegative-Flonum Nonnegative-Flonum (Option Geo-Ink))))
(define-type Geo-Calculate-Extent* (-> Geo<%> (Values Nonnegative-Flonum Nonnegative-Flonum Geo-Ink)))
(define-type Geo-Calculate-Outline (-> Geo<%> Option-Stroke-Paint Option-Stroke-Paint Geo-Pad))
(define-type Geo-Outline-Datum (U False Geo-Pad Geo-Calculate-Outline))

(struct geo-pad
  ([top : Nonnegative-Flonum]
   [right : Nonnegative-Flonum]
   [bottom : Nonnegative-Flonum]
   [left : Nonnegative-Flonum])
  #:type-name Geo-Pad
  #:transparent)

(define geo-zero-pads : Geo-Pad (geo-pad 0.0 0.0 0.0 0.0))
(define geo-pad-scale : (case-> [Geo-Pad Flonum Flonum -> Geo-Pad]
                                [Geo-Outline-Datum Flonum Flonum -> Geo-Outline-Datum])
  (lambda [self sx0 sy0]
    (if (geo-pad? self)

        (let-values ([(sx sy) (values (abs sx0) (abs sy0))])
          (geo-pad (* (geo-pad-top self)    sy)
                   (* (geo-pad-right self)  sx)
                   (* (geo-pad-bottom self) sy)
                   (* (geo-pad-left self)   sx)))

        (and self
             (λ [[master : Geo<%>] [stroke : Option-Stroke-Paint] [border : Option-Stroke-Paint]] : Geo-Pad
               (geo-pad-scale (self master stroke border) sx0 sy0))))))

(define geo-pad-expand : (-> Geo-Pad Nonnegative-Flonum Nonnegative-Flonum (Values Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [self width height]
    (define toff (geo-pad-top self))
    (define loff (geo-pad-left self))
    
    (values loff toff
            (+ width loff (geo-pad-right self))
            (+ height toff (geo-pad-bottom self)))))

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

(define geo-extent* : Geo-Calculate-Extent*
  (lambda [self]
    (define-values (width height ink) (geo-extent self))
    
    (if (not ink)
        (let*-values ([(sfc) (geo-object->surface self 1.0 cairo-create-abstract-surface*)]
                      [(ink-x ink-y ink-width ink-height) (abstract-surface-bbox sfc)])
          (values width height (make-geo-ink ink-x ink-y ink-width ink-height)))
        (values width height ink))))

(define geo-outline* : (->* (Geo<%>) (Option-Stroke-Paint Option-Stroke-Paint) Geo-Pad)
  (lambda [self [stroke (current-stroke-source)] [border (current-border-source)]]
    (define outline (geo<%>-outline self))

    (cond [(geo-pad? outline) outline]
          [(procedure? outline) (outline self stroke border)]
          [(stroke? stroke) (geo-stroke->outline stroke)]
          [else geo-zero-pads])))

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

(define geo-surface-region : (->* (Geo<%>) (Option-Stroke-Paint Option-Stroke-Paint)
                                  (Values Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [self [stroke (current-stroke-source)] [border (current-border-source)]]
    (define-values (width height) (geo-flsize self))
    (define-values (xoff yoff Width Height) (geo-pad-expand (geo-outline* self stroke border) width height))

    (values xoff yoff width height Width Height)))

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
    (define-values (xoff yoff width height Width Height) (geo-surface-region self))
  
    (stream-write /dev/strout Width Height
                  (λ [[master : Geo<%>] [vec-cr : Cairo-Ctx]
                                        [x0 : Flonum] [y0 : Flonum]
                                        [flwidth : Nonnegative-Flonum] [flheight : Nonnegative-Flonum]] : Any
                    (unless (= density 1.0)
                      (define s (/ 1.0 density))
                      (cairo_scale vec-cr s s))

                    (cairo_set_operator vec-cr (geo-operator->integer (default-pin-operator)))
                    ((geo<%>-draw! self) self vec-cr x0 y0 flwidth flheight))
                  self xoff yoff width height)))

(define #:forall (Sfc S) geo-object->surface : (-> Geo<%> Positive-Flonum
                                                   (-> Nonnegative-Flonum Nonnegative-Flonum Positive-Flonum Boolean
                                                       (Values Sfc Cairo-Ctx S S))
                                                   Sfc)
  (lambda [self density create-surface]
    (define-values (xoff yoff width height Width Height) (geo-surface-region self))
    (define-values (sfc cr fxwidth fxheight) (create-surface Width Height density #true))

    ((geo<%>-draw! self) self cr xoff yoff width height)
    (cairo_destroy cr)
    sfc))

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

(define geo-shape-outline : (case-> [Maybe-Stroke-Paint -> (Option Geo-Pad)]
                                    [Maybe-Stroke-Paint Boolean Boolean -> (Option Geo-Pad)])
  (let ([insets : (HashTable Flonum Geo-Pad) (make-hasheq)])
    (case-lambda
      [(stroke)
       (cond [(void? stroke) #false]
             [(not stroke) geo-zero-pads]
             [(stroke? stroke) (geo-stroke->outline stroke)]
             [else #false])]
      [(stroke x? y?)
       (cond [(void? stroke) #false]
             [(not stroke) geo-zero-pads]
             [(not (stroke? stroke)) #false]
             [(and x? y?) (geo-stroke->outline stroke)]
             [(or x? y?)
              (let* ([thickness (stroke-width stroke)]
                     [offset (* thickness 0.5)]
                     [hoff (if (and x?) offset 0.0)]
                     [voff (if (and y?) offset 0.0)])
                (geo-pad voff hoff voff hoff))]
             [else geo-zero-pads])])))

(define geo-stroke->outline : (-> Stroke Geo-Pad)
  (let ([insets : (HashTable Flonum Geo-Pad) (make-hasheq)])
    (lambda [stroke]
      (define thickness (stroke-width stroke))

      (hash-ref! insets thickness
                 (λ [] (let ([offset (* thickness 0.5)])
                         (geo-pad offset offset offset offset)))))))

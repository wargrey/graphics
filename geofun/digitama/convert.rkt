#lang typed/racket/base

(provide (all-defined-out))

(require "self.rkt")
(require "paint.rkt")
(require "composite.rkt")
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
          [(pen? stroke) (geo-pen->outline stroke)]
          [else geo-zero-pads])))

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

#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/paint/self)

(require geofun/digitama/unsafe/visual)
(require geofun/digitama/unsafe/surface/image)
(require geofun/digitama/unsafe/typed/cairo)

(require geofun/digitama/unsafe/stream/vector)
(require geofun/digitama/unsafe/stream/pdf)
(require geofun/digitama/unsafe/stream/svg)
(require geofun/digitama/unsafe/stream/png)

(require "self.rkt")
(require "unsafe/bitmap.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (draw-bitmap stx)
  (syntax-case stx []
    [(_ dc #:with [width height density scale? outline] [args ...] [paint-args ...])
     (syntax/loc stx
       (let*-values ([(s) (values outline)]
                     [(linewidth) (pen-maybe-width s)]
                     [(offset) (* linewidth 0.5)]
                     [(sfc png-cr fxwidth fxheight) (cairo-create-argb-image-surface* (+ linewidth width) (+ linewidth height) density scale?)])
         (dc png-cr offset offset width height args ... s paint-args ...)
         (cairo_destroy png-cr)
         (make-bitmap-from-image-surface sfc density fxwidth fxheight bitmap-convert 'dc)))]
    [(_ dc #:with [width height density scale?] args ...)
     (syntax/loc stx
       (let-values ([(sfc png-cr fxwidth fxheight) (cairo-create-argb-image-surface* width height density scale?)])
         (dc png-cr 0.0 0.0 width height args ...)
         (cairo_destroy png-cr)
         (make-bitmap-from-image-surface sfc density fxwidth fxheight bitmap-convert 'dc)))]
    [(_ dc #:with [x y width height density scale?] args ...)
     (syntax/loc stx
       (let-values ([(sfc png-cr fxwidth fxheight) (cairo-create-argb-image-surface* width height density scale?)])
         (dc png-cr x y width height args ...)
         (cairo_destroy png-cr)
         (make-bitmap-from-image-surface sfc density fxwidth fxheight bitmap-convert 'dc)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-bitmap-from-image-surface : (->* (Bitmap-Surface Positive-Flonum Positive-Index Positive-Index)
                                              (Visual-Object-Convert Symbol)
                                              Bitmap)
  (lambda [sfc density fxwidth fxheight [convert bitmap-convert] [source '/dev/ram]]
    (bitmap convert (cairo-image-shadow-size sfc) sfc source density fxwidth fxheight 4 8)))

(define create-argb-bitmap : (-> Nonnegative-Flonum Nonnegative-Flonum Positive-Flonum Boolean (Values Bitmap Cairo-Ctx))
  (lambda [width height density scale?]
    (define-values (sfc cr fxwidth fxheight) (cairo-create-argb-image-surface* width height density scale?))
    
    (values (make-bitmap-from-image-surface sfc density fxwidth fxheight) cr)))

(define create-blank-bitmap : (-> Nonnegative-Flonum Nonnegative-Flonum Positive-Flonum Bitmap)
  (lambda [width height density]
    (define-values (sfc fxwidth fxheight) (cairo-create-argb-image-surface width height density))
    
    (make-bitmap-from-image-surface sfc density fxwidth fxheight)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NOTE
; The density should only affect the displaying of bitmap
; The size of a bitmap therefore always the same as the actual size of its `surface`

(define bitmap-convert : Visual-Object-Convert
  (lambda [self mime fallback]
    (cond [(not (bitmap? self)) fallback]
          [else (let ([density (bitmap-density self)])
                  (case mime
                    [(pdf-bytes)     (bitmap->stream-bytes self 'pdf density '/dev/pdfout)]
                    [(svg-bytes)     (bitmap->stream-bytes self 'svg density '/dev/svgout)]
                    [(png@2x-bytes)  (bitmap->stream-bytes self 'png 1.0     '/dev/p2xout)]
                    [(png-bytes)     (bitmap->stream-bytes self 'png density '/dev/pngout)]
                    [(cairo-surface) (bitmap<%>-surface self)]
                    [else fallback]))])))

(define bitmap->stream-bytes : (->* (Bitmap Symbol) (Positive-Flonum Symbol) Bytes)
  (lambda [self format [density 2.0] [name #false]]
    (define /dev/sfcout : Output-Port (open-output-bytes name))
    (bitmap-surface-save (bitmap<%>-surface self) /dev/sfcout format density)
    (get-output-bytes /dev/sfcout)))

(define bitmap-surface-save : (-> Bitmap-Surface (U Path-String Output-Port) Symbol Positive-Flonum Void)
  (lambda [bmp-sfc /dev/sfcout format density]
    (case format
      [(svg) (bitmap-surface-save-with cairo-svg-stream-write bmp-sfc /dev/sfcout density)]
      [(pdf) (bitmap-surface-save-with cairo-pdf-stream-write bmp-sfc /dev/sfcout density)]
      [else  (cairo-png-stream-write /dev/sfcout (λ [] (values bmp-sfc #false)))])))

(define bitmap-surface-save-with : (-> (Cairo-Vector-Stream-Write False) Bitmap-Surface (U Path-String Output-Port) Positive-Flonum Void)
  (lambda [stream-write bmp-sfc /dev/strout density]
    (define-values (width height) (bitmap-surface-rendered-size bmp-sfc density))
  
    (stream-write /dev/strout width height
                  (λ [[master : False] [vec-cr : Cairo-Ctx]
                                       [x0 : Flonum] [y0 : Flonum]
                                       [flwidth : Nonnegative-Flonum] [flheight : Nonnegative-Flonum]] : Any
                    (unless (= density 1.0)
                      (define s (/ 1.0 density))
                      (cairo_scale vec-cr s s))
                    
                    (cairo_set_source_surface vec-cr bmp-sfc x0 y0)
                    (cairo_paint vec-cr))
                  #false 0.0 0.0 width height)))

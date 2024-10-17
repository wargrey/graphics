#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [bitmap<%>-surface bitmap-surface]))

(require geofun/digitama/unsafe/visual/object)
(require geofun/digitama/unsafe/visual/ctype)
(require geofun/digitama/unsafe/surface/image)
(require geofun/digitama/unsafe/cairo)
(require geofun/stroke)

(require geofun/digitama/unsafe/stream/vector)
(require geofun/digitama/unsafe/stream/pdf)
(require geofun/digitama/unsafe/stream/svg)
(require geofun/digitama/unsafe/stream/png)

(require "unsafe/bitmap.rkt")

(require racket/math)

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (draw-bitmap stx)
  (syntax-case stx []
    [(_ dc #:with [width height density scale? outline] [args ...] [paint-args ...])
     (syntax/loc stx
       (let*-values ([(s) outline]
                     [(linewidth) (stroke-maybe-width s)]
                     [(offset) (* linewidth 0.5)]
                     [(sfc cr fxwidth fxheight) (cairo-create-argb-image-surface* (+ linewidth width) (+ linewidth height) density scale?)])
         (dc cr offset offset width height args ... s paint-args ...)
         (make-bitmap-from-image-surface* cr sfc density fxwidth fxheight bitmap-convert 'dc)))]
    [(_ dc #:with [width height density scale?] args ...)
     (syntax/loc stx
       (let-values ([(sfc cr fxwidth fxheight) (cairo-create-argb-image-surface* width height density scale?)])
         (dc cr 0.0 0.0 width height args ...)
         (make-bitmap-from-image-surface* cr sfc density fxwidth fxheight bitmap-convert 'dc)))]
    [(_ dc #:with [x y width height density scale?] args ...)
     (syntax/loc stx
       (let-values ([(sfc cr fxwidth fxheight) (cairo-create-argb-image-surface* width height density scale?)])
         (dc cr x y width height args ...)
         (make-bitmap-from-image-surface* cr sfc density fxwidth fxheight bitmap-convert 'dc)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct bitmap<%> visual-object<%>
  ([shadow : Phantom-Bytes]
   [surface : Bitmap-Surface])
  #:type-name Bitmap<%>)

(struct bitmap bitmap<%>
  ([source : Symbol]
   [density : Positive-Flonum]
   [intrinsic-width : Positive-Index]
   [intrinsic-height : Positive-Index]
   [palettes : Positive-Byte]
   [depth : Positive-Byte])
  #:type-name Bitmap
  #:transparent)

(define make-bitmap-from-image-surface : (->* (Bitmap-Surface Positive-Flonum Positive-Index Positive-Index)
                                              (Visual-Object-Convert Symbol)
                                              Bitmap)
  (lambda [sfc density fxwidth fxheight [convert bitmap-convert] [source '/dev/ram]]
    (bitmap convert (cairo-image-shadow-size sfc) sfc source density fxwidth fxheight 4 8)))

(define make-bitmap-from-image-surface* : (->* (Cairo-Ctx Bitmap-Surface Positive-Flonum Positive-Index Positive-Index)
                                               (Visual-Object-Convert Symbol)
                                               Bitmap)
  (lambda [cr sfc density fxwidth fxheight [convert bitmap-convert] [source '/dev/ram]]
    (cairo_destroy cr)
    (make-bitmap-from-image-surface sfc density fxwidth fxheight convert source)))

(define create-argb-bitmap : (-> Nonnegative-Flonum Nonnegative-Flonum Positive-Flonum Boolean (Values Bitmap Cairo-Ctx))
  (lambda [width height density scale?]
    (define-values (sfc cr fxwidth fxheight) (cairo-create-argb-image-surface* width height density scale?))
    (values (make-bitmap-from-image-surface sfc density fxwidth fxheight) cr)))

(define create-blank-bitmap : (-> Nonnegative-Flonum Nonnegative-Flonum Positive-Flonum Bitmap)
  (lambda [width height density]
    (define-values (sfc fxwidth fxheight) (cairo-create-image-surface width height density))
    (make-bitmap-from-image-surface sfc density fxwidth fxheight)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-width : (-> Bitmap Index)
  (lambda [bmp]
    (assert (exact-ceiling (/ (bitmap-intrinsic-width bmp) (bitmap-density bmp))) index?)))

(define bitmap-height : (-> Bitmap Index)
  (lambda [bmp]
    (assert (exact-ceiling (/ (bitmap-intrinsic-height bmp) (bitmap-density bmp))) index?)))

(define bitmap-size : (-> Bitmap (Values Index Index))
  (lambda [bmp]
    (define-values (flw flh) (bitmap-flsize bmp))
    
    (values (assert (exact-ceiling flw) index?)
            (assert (exact-ceiling flh) index?))))

(define bitmap-depth* : (-> Bitmap (Values Positive-Byte Positive-Byte))
  (lambda [bmp]
    (values (bitmap-depth bmp)
            (bitmap-palettes bmp))))

(define bitmap-intrinsic-size : (-> Bitmap (Values Positive-Index Positive-Index))
  (lambda [bmp]
    (values (bitmap-intrinsic-width bmp)
            (bitmap-intrinsic-height bmp))))

(define bitmap-intrinsic-size+density : (-> Bitmap (Values Positive-Index Positive-Index Positive-Flonum))
  (lambda [bmp]
    (define-values (fxw fxh) (bitmap-intrinsic-size bmp))
    (values fxw fxh (bitmap-density bmp))))

(define bitmap-data : (-> Bitmap Bytes)
  (lambda [bmp]
    (define-values (pixels _) (bitmap-surface-data (bitmap<%>-surface bmp)))
    pixels))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-flsize : (case-> [Bitmap -> (Values Nonnegative-Flonum Nonnegative-Flonum)]
                                [Bitmap Nonnegative-Real -> (Values Nonnegative-Flonum Nonnegative-Flonum)]
                                [Bitmap Nonnegative-Real Nonnegative-Real -> (Values Nonnegative-Flonum Nonnegative-Flonum)])
  (let ([flsize (λ [[bmp : Bitmap] [w% : Nonnegative-Flonum] [h% : Nonnegative-Flonum]] : (Values Nonnegative-Flonum Nonnegative-Flonum)
                  (let-values ([(w h) (bitmap-flsize bmp)])
                    (values (* w w%) (* h h%))))])
    (case-lambda
      [(bmp w% h%) (flsize bmp (real->double-flonum w%) (real->double-flonum h%))]
      [(bmp ratio) (let ([% (real->double-flonum ratio)]) (flsize bmp % %))]
      [(bmp) (let-values ([(flw flh density) (bitmap-intrinsic-flsize+density bmp)])
               (values (/ flw density) (/ flh density)))])))

(define bitmap-intrinsic-flsize : (-> Bitmap (Values Positive-Flonum Positive-Flonum))
  (lambda [bmp]
    (define-values (w h) (bitmap-intrinsic-size bmp))
    (values (exact->inexact w) (exact->inexact h))))

(define bitmap-intrinsic-flsize+density : (-> Bitmap (Values Positive-Flonum Positive-Flonum Positive-Flonum))
  (lambda [bmp]
    (define-values (flw flh) (bitmap-intrinsic-flsize bmp))
    (values flw flh (bitmap-density bmp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NOTE
; The density should only affect the displaying of bitmap
; The size of a bitmap therefore always the same as the actual size of its `surface`

(define bitmap-convert : Visual-Object-Convert
  (lambda [self mime fallback]
    (cond [(not (bitmap? self)) fallback]
          [else (let ([density (bitmap-density self)])
                  (define surface (bitmap<%>-surface self))
                  (case mime
                    [(pdf-bytes) (bitmap-surface->stream-bytes surface 'pdf '/dev/pdfout density)]
                    [(svg-bytes) (bitmap-surface->stream-bytes surface 'svg '/dev/svgout density)]
                    [(png@2x-bytes) (bitmap-surface->stream-bytes surface 'png '/dev/p2xout 1.0)]
                    [(png-bytes) (bitmap-surface->stream-bytes surface 'png '/dev/pngout density)]
                    [(cairo-surface) surface]
                    [else fallback]))])))

(define bitmap-surface->stream-bytes : (-> Bitmap-Surface Symbol Symbol Positive-Flonum Bytes)
  (lambda [sfc format name density]
    (define /dev/sfcout : Output-Port (open-output-bytes name))
    (bitmap-surface-save sfc /dev/sfcout format density)
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
                  #false 0.0 0.0)))

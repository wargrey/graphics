#lang typed/racket/base

(provide (all-defined-out) Bitmap-Surface bitmap-surface?)
(provide (rename-out [Bitmap<%>-surface bitmap-surface]))
(provide (rename-out [Bitmap<%>-density bitmap-density]))
(provide (rename-out [Bitmap-width bitmap-width]))
(provide (rename-out [Bitmap-height bitmap-height]))
(provide (rename-out [Bitmap? bitmap?]))

(require typed/racket/unsafe)
(require file/convertible)
(require racket/fixnum)
(require racket/flonum)

(unsafe-provide (rename-out [create-argb-bitmap create-bitmap]))

(module unsafe racket/base
  (provide (all-defined-out))
  (provide (rename-out [cpointer? bitmap-surface?]))

  (require ffi/unsafe)
  (require ffi/unsafe/atomic)
  (require racket/unsafe/ops)
  (require racket/draw/unsafe/cairo)
  (require racket/draw/unsafe/bstr)
  
  (define (cairo-surface-intrinsic-size sfs)
    (values (cairo_image_surface_get_width sfs)
            (cairo_image_surface_get_height sfs)))

  (define (cairo-surface-size sfs density)
    (define-values (width height) (cairo-surface-intrinsic-size sfs))
    (values (unsafe-fl/ (unsafe-fx->fl width) density)
            (unsafe-fl/ (unsafe-fx->fl height) density)))

  (define (cairo-surface-data sfs)
    (define data (cairo_image_surface_get_data sfs))
    (values data (unsafe-bytes-length data)))

  (define (cairo-surface-metrics sfs components)
    (define-values (data total) (cairo-surface-data sfs))
    (define stride (cairo_image_surface_get_stride sfs))
    (values data total stride
            (unsafe-fxquotient stride components)
            (unsafe-fxquotient total stride)))

  (define (cairo-surface->png-bytes s)
    (define /dev/pngout (open-output-bytes '/dev/pngout))
    (define (do-write ignored bstr len)
      (write-bytes (scheme_make_sized_byte_string bstr len 0) /dev/pngout)
      CAIRO_STATUS_SUCCESS)
    (start-breakable-atomic)
    (cairo_surface_write_to_png_stream s do-write)
    (end-breakable-atomic)
    (get-output-bytes /dev/pngout)))

(unsafe-require/typed
 (submod "." unsafe)
 [#:opaque Bitmap-Surface bitmap-surface?]
 [cairo-surface-intrinsic-size (-> Bitmap-Surface (Values Positive-Fixnum Positive-Fixnum))]
 [cairo-surface->png-bytes (-> Bitmap-Surface Bytes)])

(struct Bitmap<%>
  ([convert : (Option Procedure)]
   [pixman : Bytes]
   [surface : Bitmap-Surface]
   [density : Positive-Flonum])
  #:property prop:convertible
  (λ [self mime fallback]
    (with-handlers ([exn? (λ [e : exn] (invalid-convert self mime fallback))])
      (define convert (or (Bitmap<%>-convert self) graphics-convert))
      (convert self mime fallback))))

(struct Bitmap Bitmap<%>
  ([srcname : Symbol]
   [width : Positive-Index]
   [height : Positive-Index]
   [palettes : Positive-Byte]
   [depth : Positive-Byte])
  #:transparent)

(define create-argb-bitmap : (-> Bytes Bitmap-Surface Positive-Index Positive-Index Positive-Flonum Bitmap)
  (lambda [pixman surface width height density]
    (Bitmap #false pixman surface density '/dev/ram width height 4 8)))

(define bitmap-size : (-> Bitmap (Values Positive-Index Positive-Index))
  (lambda [bmp]
    (values (Bitmap-width bmp)
            (Bitmap-height bmp))))

(define bitmap-size+density : (-> Bitmap (Values Positive-Index Positive-Index Positive-Flonum))
  (lambda [bmp]
    (values (Bitmap-width bmp)
            (Bitmap-height bmp)
            (Bitmap<%>-density bmp))))

(define bitmap-depth : (-> Bitmap (Values Positive-Byte Positive-Byte))
  (lambda [bmp]
    (values (Bitmap-depth bmp)
            (Bitmap-palettes bmp))))

(define bitmap-intrinsic-size : (-> Bitmap (Values Positive-Fixnum Positive-Fixnum))
  (lambda [bmp]
    (cairo-surface-intrinsic-size (Bitmap<%>-surface bmp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-flsize : (case-> [Bitmap -> (Values Positive-Flonum Positive-Flonum)]
                                [Bitmap Nonnegative-Real -> (Values Nonnegative-Flonum Nonnegative-Flonum)]
                                [Bitmap Nonnegative-Real Nonnegative-Real -> (Values Nonnegative-Flonum Nonnegative-Flonum)])
  (let ([flsize : (-> Bitmap Nonnegative-Flonum Nonnegative-Flonum (Values Nonnegative-Flonum Nonnegative-Flonum))
                 (λ [bmp w% h%] (values (fl* (->fl (Bitmap-width bmp)) w%) (fl* (->fl (Bitmap-width bmp)) h%)))])
    (case-lambda [(bmp) (let-values ([(w h) (bitmap-size bmp)]) (values (exact->inexact w) (exact->inexact h)))]
                 [(bmp ratio) (let ([% (real->double-flonum ratio)]) (flsize bmp % %))]
                 [(bmp w% h%) (flsize bmp (real->double-flonum w%) (real->double-flonum h%))])))

(define bitmap-intrinsic-flsize : (-> Bitmap (Values Positive-Flonum Positive-Flonum))
  (lambda [bmp]
    (define-values (w h) (bitmap-intrinsic-size bmp))
    (values (fx->fl w) (fx->fl h))))

(define bitmap-flsize+density : (-> Bitmap (Values Positive-Flonum Positive-Flonum Positive-Flonum))
  (lambda [bmp]
    (values (fx->fl (Bitmap-width bmp))
            (fx->fl (Bitmap-height bmp))
            (Bitmap<%>-density bmp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define graphics-convert : (-> Bitmap Symbol Any Any)
  (lambda [self mime fallback]
    (define density (Bitmap<%>-density self))
    (define surface (Bitmap<%>-surface self))
    (case mime
      [(png@2x-bytes) (or (and (fl= density 2.0) (cairo-surface->png-bytes surface)) fallback)]
      [(png-bytes) (cairo-surface->png-bytes surface)]
      [else fallback])))

(define invalid-convert : (-> Bitmap Symbol Any Any)
  (lambda [self mime fallback]
    #""))

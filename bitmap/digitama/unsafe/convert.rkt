#lang typed/racket/base

(provide (except-out (all-defined-out) create-argb-bitmap create-invalid-bitmap))
(provide Bitmap-Surface bitmap-surface? cairo-surface-shadow)
(provide (rename-out [bitmap<%>-surface bitmap-surface]))

(require typed/racket/unsafe)
(require file/convertible)

(unsafe-provide create-argb-bitmap create-invalid-bitmap cairo-surface-save)

(module unsafe racket/base
  (provide (all-defined-out) phantom-bytes? make-phantom-bytes)
  (provide (rename-out [cpointer? bitmap-surface?]))

  (require ffi/unsafe)
  (require ffi/unsafe/atomic)
  
  (require racket/unsafe/ops)
  (require racket/draw/unsafe/cairo)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
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

  (define (cairo-surface-shadow sfs)
    (make-phantom-bytes (unsafe-bytes-length (cairo_image_surface_get_data sfs))))

  (define (cairo-surface->stream-bytes sfs format name density)
    (define /dev/sfsout (open-output-bytes name))
    (cairo-surface-save sfs /dev/sfsout format density)
    (get-output-bytes /dev/sfsout))

  (define (cairo-surface-save sfs /dev/sfsout format density)
    (start-breakable-atomic)
    (case format
      [(svg) (cairo-surface-save-as-svg sfs /dev/sfsout density)]
      [(pdf) (cairo-surface-save-as-pdf sfs /dev/sfsout density)]
      [else  (cairo-surface-save-as-png sfs /dev/sfsout density)])
    (end-breakable-atomic))

  (define (cairo-surface-save-as-png sfs /dev/pngout density)
    (define (do-write ignored bstr-ptr len) (cairo-write bstr-ptr len /dev/pngout))

    (cairo_surface_write_to_png_stream sfs do-write))

  (define (cairo-surface-save-as-svg sfs /dev/svgout density)
    (define (do-write bstr-ptr len) (cairo-write bstr-ptr len /dev/svgout))
    (define-values (w h) (cairo-surface-size sfs density))
    (define svg-surface (cairo_svg_surface_create_for_stream do-write w h))
    (define svg-cr (cairo_create svg-surface))

    (unless (unsafe-fl= density 1.0)
      (define s (unsafe-fl/ 1.0 density))
      (cairo_scale svg-cr s s))

    (cairo_set_source_surface svg-cr sfs 0.0 0.0)
    (cairo_paint svg-cr)
    (cairo_surface_flush svg-surface)
    (cairo_surface_finish svg-surface)
    (cairo_destroy svg-cr))

  (define (cairo-surface-save-as-pdf sfs /dev/pdfout density)
    (define (do-write bstr-ptr len) (cairo-write bstr-ptr len /dev/pdfout))
    (define-values (w h) (cairo-surface-size sfs density))
    (define pdf-surface (cairo_pdf_surface_create_for_stream do-write w h))
    (define pdf-cr (cairo_create pdf-surface))

    (unless (unsafe-fl= density 1.0)
      (define s (unsafe-fl/ 1.0 density))
      (cairo_scale pdf-cr s s))
    
    (cairo_set_source_surface pdf-cr sfs 0.0 0.0)
    (cairo_paint pdf-cr)
    (cairo_surface_flush pdf-surface)
    (cairo_surface_finish pdf-surface)
    (cairo_destroy pdf-cr))

  (define (cairo-write bstr-ptr len /dev/bmpout)
    (define bstr (make-bytes len))
    
    (memcpy bstr bstr-ptr len)
    (write-bytes bstr /dev/bmpout)
    CAIRO_STATUS_SUCCESS))

(unsafe-require/typed
 (submod "." unsafe)
 [#:opaque Bitmap-Surface bitmap-surface?]
 [#:opaque Phantom-Bytes phantom-bytes?]
 [cairo-surface-intrinsic-size (-> Bitmap-Surface (Values Positive-Fixnum Positive-Fixnum))]
 [cairo-surface-data (-> Bitmap-Surface (Values Bytes Index))]
 [cairo-surface-shadow (-> Bitmap-Surface Phantom-Bytes)]
 [cairo-surface->stream-bytes (-> Bitmap-Surface Symbol Symbol Flonum Bytes)]
 [cairo-surface-save (-> Bitmap-Surface Output-Port Symbol Void)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Visual-Object-Convert (-> Visual-Object<%> Symbol Any Any))

(struct visual-object<%>
  ([convert : (Option Visual-Object-Convert)])
  #:type-name Visual-Object<%>
  #:property prop:convertible
  (λ [[self : Visual-Object<%>] [mime : Symbol] [fallback : Any]]
    (define maybe-convert (visual-object<%>-convert self))

    (cond [(not maybe-convert) (graphics-convert self mime fallback)]
          [else (let* ([sentry (gensym 'convert)]
                       [datum (maybe-convert self mime sentry)])
                  (cond [(eq? datum sentry) (graphics-convert self mime fallback)]
                        [(bitmap<%>? datum) (graphics-convert datum mime fallback)]
                        [else datum]))])))

(struct bitmap<%> visual-object<%>
  ([shadow : Phantom-Bytes]
   [surface : Bitmap-Surface])
  #:type-name Bitmap<%>)

(struct bitmap bitmap<%>
  ([source : Symbol]
   [density : Positive-Flonum]
   [width : Positive-Index]
   [height : Positive-Index]
   [palettes : Positive-Byte]
   [depth : Positive-Byte])
  #:type-name Bitmap
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define create-argb-bitmap : (-> Bitmap-Surface Positive-Index Positive-Index Positive-Flonum Bitmap)
  (lambda [surface width height density]
    (bitmap #false (cairo-surface-shadow surface) surface '/dev/ram density width height 4 8)))

(define create-invalid-bitmap : (-> Bitmap-Surface Positive-Index Positive-Index Positive-Flonum Bitmap)
  (lambda [surface width height density]
    (bitmap invalid-convert (cairo-surface-shadow surface) surface (string->uninterned-symbol "/dev/zero") density width height 4 8)))

(define bitmap-size : (-> Bitmap (Values Positive-Index Positive-Index))
  (lambda [bmp]
    (values (bitmap-width bmp)
            (bitmap-height bmp))))

(define bitmap-size+density : (-> Bitmap (Values Positive-Index Positive-Index Positive-Flonum))
  (lambda [bmp]
    (values (bitmap-width bmp)
            (bitmap-height bmp)
            (bitmap-density bmp))))

(define bitmap-depth* : (-> Bitmap (Values Positive-Byte Positive-Byte))
  (lambda [bmp]
    (values (bitmap-depth bmp)
            (bitmap-palettes bmp))))

(define bitmap-intrinsic-size : (-> Bitmap (Values Positive-Fixnum Positive-Fixnum))
  (lambda [bmp]
    (cairo-surface-intrinsic-size (bitmap<%>-surface bmp))))

(define bitmap-data : (-> Bitmap Bytes)
  (lambda [bmp]
    (define-values (pixels _) (cairo-surface-data (bitmap<%>-surface bmp)))
    pixels))

(define bitmap-invalid? : (-> Bitmap Boolean)
  (lambda [bmp]
    (let ([src (bitmap-source bmp)])
      (not (or (symbol-interned? src)
               (symbol-unreadable? src))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-flsize : (case-> [Bitmap -> (Values Positive-Flonum Positive-Flonum)]
                                [Bitmap Nonnegative-Real -> (Values Nonnegative-Flonum Nonnegative-Flonum)]
                                [Bitmap Nonnegative-Real Nonnegative-Real -> (Values Nonnegative-Flonum Nonnegative-Flonum)])
  (let ([flsize (λ [[bmp : Bitmap] [w% : Nonnegative-Flonum] [h% : Nonnegative-Flonum]] : (Values Nonnegative-Flonum Nonnegative-Flonum)
                  (let-values ([(w h) (bitmap-flsize bmp)]) (values (* w w%) (* h h%))))])
    (case-lambda [(bmp) (values (exact->inexact (bitmap-width bmp)) (exact->inexact (bitmap-height bmp)))]
                 [(bmp ratio) (let ([% (real->double-flonum ratio)]) (flsize bmp % %))]
                 [(bmp w% h%) (flsize bmp (real->double-flonum w%) (real->double-flonum h%))])))

(define bitmap-intrinsic-flsize : (-> Bitmap (Values Positive-Flonum Positive-Flonum))
  (lambda [bmp]
    (define-values (w h) (bitmap-intrinsic-size bmp))
    (values (exact->inexact w) (exact->inexact h))))

(define bitmap-flsize+density : (-> Bitmap (Values Positive-Flonum Positive-Flonum Positive-Flonum))
  (lambda [bmp]
    (define-values (flw flh) (bitmap-flsize bmp))
    (values flw flh (bitmap-density bmp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define graphics-convert : Visual-Object-Convert
  (lambda [self mime fallback]
    (cond [(not (bitmap? self)) fallback]
          [else (let ([density (bitmap-density self)])
                  (define surface (bitmap<%>-surface self))
                  (case mime
                    [(png@2x-bytes) (or (and (= density 2.0) (cairo-surface->stream-bytes surface 'png '/dev/p2xout 1.0)) fallback)]
                    [(png-bytes) (cairo-surface->stream-bytes surface 'png '/dev/pngout density)]
                    [(svg-bytes) (cairo-surface->stream-bytes surface 'svg '/dev/svgout density)]
                    [(pdf-bytes) (cairo-surface->stream-bytes surface 'pdf '/dev/pdfout density)]
                    [else fallback]))])))

(define invalid-convert : Visual-Object-Convert
  (lambda [self mime fallback]
    #""))

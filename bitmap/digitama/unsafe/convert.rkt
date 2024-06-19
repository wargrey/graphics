#lang typed/racket/base

(provide (all-defined-out))
(provide Phantom-Bytes Bitmap-Surface)
(provide bitmap-surface? bitmap-surface-save)

(provide (rename-out [bitmap<%>-surface bitmap-surface]))

(require "visual/object.rkt")
(require "visual/bitmap.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(define bitmap-intrinsic-size : (-> Bitmap (Values Positive-Index Positive-Index))
  (lambda [bmp]
    (bitmap-surface-intrinsic-size (bitmap<%>-surface bmp))))

(define bitmap-data : (-> Bitmap Bytes)
  (lambda [bmp]
    (define-values (pixels _) (bitmap-surface-data (bitmap<%>-surface bmp)))
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
                  (let-values ([(w h) (bitmap-flsize bmp)])
                    (values (* w w%) (* h h%))))])
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
                    [else fallback]))])))

(define invalid-convert : Visual-Object-Convert
  (lambda [self mime fallback]
    #""))

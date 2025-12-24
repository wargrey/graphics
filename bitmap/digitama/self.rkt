#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [bitmap<%>-surface bitmap-surface]))

(require geofun/digitama/unsafe/visual)
(require geofun/digitama/unsafe/typed/cairo)

(require "unsafe/bitmap.rkt")

(require racket/math)

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

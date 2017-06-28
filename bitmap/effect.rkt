#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/draw.rkt")
(require "digitama/unsafe/draw.rkt")
(require "digitama/unsafe/effect.rkt")

(define bitmap-cellophane : (-> Bitmap Nonnegative-Real Bitmap)
  (lambda [bmp opacity]
    (define alpha : Flonum (real->double-flonum opacity))
    (if (fl>= alpha 1.0) bmp (bitmap_cellophane (bitmap-surface bmp) alpha (bitmap-density bmp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-grayscale : (-> Bitmap (-> Byte Byte Byte Integer) Bitmap)
  (lambda [bmp rgb->gray]
    (bitmap_grayscale (bitmap-surface bmp) rgb->gray (bitmap-density bmp))))

(define bitmap-grayscale/lightness : (-> Bitmap Bitmap)
  (lambda [bmp]
    (bitmap-grayscale bmp
                      (λ [[r : Byte] [g : Byte] [b : Byte]] : Integer
                        (fxquotient (fx+ (fxmax (fxmax r g) b)
                                         (fxmin (fxmin r g) b))
                                    2)))))

(define bitmap-grayscale/average : (-> Bitmap Bitmap)
  (lambda [bmp]
    (bitmap-grayscale bmp
                      (λ [[r : Byte] [g : Byte] [b : Byte]] : Integer
                        (fxquotient (fx+ (fx+ r g) b) 3)))))

(define bitmap-grayscale/luminosity : (->* (Bitmap) (Nonnegative-Real Nonnegative-Real Nonnegative-Real) Bitmap)
  (lambda [bmp [ro 0.2126729] [go 0.7151522] [bo 0.0721750]]
    (define flr : Flonum (real->double-flonum ro))
    (define flg : Flonum (real->double-flonum go))
    (define flb : Flonum (real->double-flonum bo))
    (bitmap-grayscale bmp
                      (λ [[r : Byte] [g : Byte] [b : Byte]] : Integer
                        (define gr : Flonum (fl* (fx->fl r) flr))
                        (define gg : Flonum (fl* (fx->fl g) flg))
                        (define gb : Flonum (fl* (fx->fl b) flb))
                        (fxmin (fl->fx (flround (fl+ (fl+ gr gg) gb))) 255)))))

(define bitmap-grayscale/decomposition : (-> Bitmap (U 'max 'min) Bitmap)
  (lambda [bmp algorithm]
    (bitmap-grayscale bmp
                      (if (eq? algorithm 'min)
                          (λ [[r : Byte] [g : Byte] [b : Byte]] : Integer
                            (fxmin (fxmin r g) b))
                          (λ [[r : Byte] [g : Byte] [b : Byte]] : Integer
                            (fxmax (fxmax r g) b))))))

(define bitmap-grayscale/channel : (-> Bitmap (U 'red 'green 'blue) Bitmap)
  (lambda [bmp channel]
    (bitmap-grayscale bmp
                      (case channel
                        [(red) (λ [[r : Byte] [g : Byte] [b : Byte]] : Byte r)]
                        [(green) (λ [[r : Byte] [g : Byte] [b : Byte]] : Byte g)]
                        [else (λ [[r : Byte] [g : Byte] [b : Byte]] : Byte b)]))))

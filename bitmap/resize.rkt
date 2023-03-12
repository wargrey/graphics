#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/resize.rkt")
(require "digitama/unsafe/resize.rkt")
(require "digitama/unsafe/convert.rkt")

(define bitmap-section : (case-> [Bitmap Complex Complex -> Bitmap]
                                 [Bitmap Complex Nonnegative-Real Nonnegative-Real -> Bitmap]
                                 [Bitmap Real Real Nonnegative-Real Nonnegative-Real -> Bitmap])
  (case-lambda
    [(bmp pt0 ptn)
     (define delta : Complex (- ptn pt0))
     (bitmap-section bmp (real-part pt0) (imag-part pt0) (max (real-part delta) 0) (max (imag-part delta) 0))]
    [(bmp pt width height) (bitmap-section bmp (real-part pt) (imag-part pt) width height)]
    [(bmp x y width height)
     (define-values (src-width src-height) (bitmap-flsize bmp))
     (cond [(and (zero? x) (zero? y) (= width src-width) (= height src-height)) bmp]
           [else (bitmap_section (bitmap-surface bmp) (real->double-flonum x) (real->double-flonum y)
                                 (min (real->double-flonum width) src-width) (min (real->double-flonum height) src-height)
                                 (bitmap-density bmp))])]))

(define bitmap-copy : (case-> [Bitmap -> Bitmap]
                              [Bitmap Complex Nonnegative-Real Nonnegative-Real -> Bitmap]
                              [Bitmap Real Real Nonnegative-Real Nonnegative-Real -> Bitmap])
  (case-lambda
    [(bmp)
     (define-values (src-width src-height) (bitmap-flsize bmp))
     (bitmap_section (bitmap-surface bmp) 0.0 0.0 src-width src-height (bitmap-density bmp))]
    [(bmp pt width height)
     (define-values (src-width src-height) (bitmap-flsize bmp))
     (bitmap_section (bitmap-surface bmp) (real->double-flonum (real-part pt)) (real->double-flonum (imag-part pt))
                     (min (real->double-flonum width) src-width) (min (real->double-flonum height) src-height)
                     (bitmap-density bmp))]
    [(bmp x y width height)
     (define-values (src-width src-height) (bitmap-flsize bmp))
     (bitmap_section (bitmap-surface bmp) (real->double-flonum x) (real->double-flonum y)
                     (min (real->double-flonum width) src-width) (min (real->double-flonum height) src-height)
                     (bitmap-density bmp))]))

(define bitmap-bounding-box : (->* (Bitmap) (Boolean)
                                    (Values Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [bmp [just-alpha? #true]]
    (bitmap_bounding_box* (bitmap-surface bmp) just-alpha? (bitmap-density bmp))))

(define bitmap-trim : (->* (Bitmap) (Boolean) Bitmap)
  (lambda [bmp [just-alpha? #true]]
    (define surface : Bitmap-Surface (bitmap-surface bmp))
    (define density : Flonum (bitmap-density bmp))
    (define-values (x y X Y) (bitmap_bounding_box* surface just-alpha? 1.0))
    (bitmap_section surface x y (- X x) (- Y y) density)))

(define bitmap-inset : (case-> [Bitmap Real -> Bitmap]
                               [Bitmap Real Real -> Bitmap]
                               [Bitmap Real Real Real Real -> Bitmap])
  (case-lambda
    [(bmp inset)
     (bitmap-inset bmp inset inset inset inset)]
    [(bmp vertical horizontal)
     (bitmap-inset bmp vertical horizontal vertical horizontal)]
    [(bmp top right bottom left)
     (define-values (flwidth flheight density) (bitmap-flsize+density bmp))
     (define-values (flleft flright) (values (* (real->double-flonum left) density) (* (real->double-flonum right) density)))
     (define-values (fltop flbottom) (values (* (real->double-flonum top) density) (* (real->double-flonum bottom) density)))
     (bitmap_section (bitmap-surface bmp) (- flleft) (- fltop)
                     (+ flwidth flright flleft) (+ flheight flbottom fltop)
                     density)]))

(define-cropper bitmap-crop : (-> Bitmap Positive-Real Positive-Real Bitmap)
  (#:lambda [bmp width height left% top%]
    (define-values (W H density) (bitmap-flsize+density bmp))
    (define w (min W (* (real->double-flonum width) density)))
    (define h (min H (* (real->double-flonum height) density)))
    (bitmap_section (bitmap-surface bmp) (* (- W w) left%) (* (- H h) top%) w h density))
  #:with ("bitmap-~a-crop" [lt 0.0 0.0] [lc 0.0 0.5] [lb 0.0 1.0]
                           [ct 0.5 0.0] [cc 0.5 0.5] [cb 0.5 1.0]
                           [rt 1.0 0.0] [rc 1.0 0.5] [rb 1.0 1.0]))

(define bitmap-scale : (->* (Bitmap Real) (Real) Bitmap)
  (case-lambda
    [(bmp scale)
     (if (= scale 1.0) bmp (bitmap-scale bmp scale scale))]
    [(bmp scale-x scale-y)
     (cond [(and (= scale-x 1.0) (= scale-y 1.0)) bmp]
           [else (bitmap_scale (bitmap-surface bmp)
                               (real->double-flonum scale-x)
                               (real->double-flonum scale-y)
                               (bitmap-density bmp))])]))

(define bitmap-resize : (case-> [Bitmap (U Bitmap Nonnegative-Real) -> Bitmap]
                                [Bitmap Nonnegative-Real Nonnegative-Real -> Bitmap])
  (case-lambda
    [(bmp refer)
     (if (not (real? refer))
         (bitmap-resize bmp (bitmap-width refer) (bitmap-height refer))
         (bitmap-scale bmp (/ (real->double-flonum refer)
                              (exact->inexact (min (bitmap-width bmp)
                                                   (bitmap-height bmp))))))]
    [(bmp w h)
     (bitmap-scale bmp
                   (/ (real->double-flonum w) (exact->inexact (bitmap-width bmp)))
                   (/ (real->double-flonum h) (exact->inexact (bitmap-height bmp))))]))

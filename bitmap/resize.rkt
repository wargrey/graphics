#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/self.rkt")
(require "digitama/unsafe/adjust.rkt")

(require geofun/digitama/resize)
(require geofun/digitama/geometry/insets)
(require geofun/digitama/unsafe/typed/c)

(require digimon/measure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-section : (case-> [Bitmap Complex Complex -> Bitmap]
                                 [Bitmap Complex Real Real -> Bitmap]
                                 [Bitmap Real Real Real Real -> Bitmap])
  (case-lambda
    [(bmp pt0 ptn) (let ([delta (- ptn pt0)]) (bitmap-section bmp (real-part pt0) (imag-part pt0) (real-part delta) (imag-part delta)))]
    [(bmp pt width height) (bitmap-section bmp (real-part pt) (imag-part pt) width height)]
    [(bmp x y width height)
     (let-values ([(src-width src-height) (bitmap-intrinsic-flsize bmp)])
       (cond [(and (zero? x) (zero? y) (= width src-width) (= height src-height)) bmp]
             [else (bitmap_section (bitmap-surface bmp) (real->double-flonum x) (real->double-flonum y)
                                   (min (real->double-flonum width) src-width) (min (real->double-flonum height) src-height)
                                   (bitmap-density bmp))]))]))

(define bitmap-copy : (case-> [Bitmap -> Bitmap]
                              [Bitmap Complex Real Real -> Bitmap]
                              [Bitmap Real Real Real Real -> Bitmap])
  (case-lambda
    [(bmp) (let-values ([(src-width src-height) (bitmap-intrinsic-flsize bmp)]) (bitmap-copy bmp 0.0 0.0 src-width src-height))]
    [(bmp pt width height) (bitmap-copy bmp (real-part pt) (imag-part pt) width height)]
    [(bmp x y width height)
     (let-values ([(src-width src-height) (bitmap-intrinsic-flsize bmp)])
       (bitmap_section (bitmap-surface bmp) (real->double-flonum x) (real->double-flonum y)
                       (min (real->double-flonum width) src-width) (min (real->double-flonum height) src-height)
                       (bitmap-density bmp)))]))

(define bitmap-bounding-box : (->* (Bitmap) (Boolean)
                                   (Values Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [bmp [just-alpha? #true]]
    (bitmap_bounding_box* (bitmap-surface bmp) just-alpha? (bitmap-density bmp))))

(define bitmap-trim : (->* (Bitmap) (Boolean) Bitmap)
  (lambda [bmp [just-alpha? #true]]
    (define surface : Bitmap-Surface (bitmap-surface bmp))
    (define density : Positive-Flonum (bitmap-density bmp))
    (define-values (x y X Y) (bitmap_bounding_box* surface just-alpha? 1.0))
    (bitmap_section surface x y (- X x) (- Y y) density)))

(define bitmap-inset : (case-> [Bitmap -> Bitmap]
                               [Bitmap Real -> Bitmap]
                               [Bitmap Real Real -> Bitmap]
                               [Bitmap Real Real Real -> Bitmap]
                               [Bitmap Real Real Real Real -> Bitmap])
  (case-lambda
    [(bmp inset) (bitmap-inset bmp inset inset inset inset)]
    [(bmp vertical horizontal) (bitmap-inset bmp vertical horizontal vertical horizontal)]
    [(self top horizontal bottom) (bitmap-inset self top horizontal bottom horizontal)]
    [(bmp top right bottom left)
     (let*-values ([(flwidth flheight density) (bitmap-intrinsic-flsize+density bmp)]
                   [(flleft flright) (values (* (real->double-flonum left) density) (* (real->double-flonum right) density))]
                   [(fltop flbottom) (values (* (real->double-flonum top) density) (* (real->double-flonum bottom) density))])
       (bitmap_section (bitmap-surface bmp) (- flleft) (- fltop)
                       (+ flwidth flright flleft) (+ flheight flbottom fltop)
                       density))]
    [(bmp)
     (let-values ([(flw flh) (bitmap-flsize bmp)])
       (cond [(= flw flh) bmp]
             [(< flw flh) (bitmap-inset bmp 0.0 (* (- flh flw) 0.5))]
             [else (bitmap-inset bmp (* (- flw flh) 0.5) 0.0)]))]))

(define-cropper bitmap-crop : (-> Bitmap Nonnegative-Real Nonnegative-Real Bitmap)
  (#:lambda [bmp width height left% top%]
   (define-values (W H density) (bitmap-intrinsic-flsize+density bmp))
   (define w (min W (* (real->double-flonum width) density)))
   (define h (min H (* (real->double-flonum height) density)))
   (bitmap_section (bitmap-surface bmp) (* (- W w) left%) (* (- H h) top%) w h density))
  #:with ("bitmap-~a-crop" [lt 0.0 0.0] [lc 0.0 0.5] [lb 0.0 1.0]
                           [ct 0.5 0.0] [cc 0.5 0.5] [cb 0.5 1.0]
                           [rt 1.0 0.0] [rc 1.0 0.5] [rb 1.0 1.0]))

(define bitmap-rotate : (case-> [Bitmap Real -> Bitmap]
                                [Bitmap Real Angle-Unit -> Bitmap])
  (case-lambda
    [(bmp theta)
     (cond [(= theta 0.0) bmp]
           [else (bitmap_rotate (bitmap-surface bmp) (real->double-flonum theta) (bitmap-density bmp))])]
    [(bmp theta unit) (bitmap-rotate bmp (~rad theta unit))]))

(define bitmap-skew : (case-> [Bitmap Real Real -> Bitmap]
                              [Bitmap Real Real Angle-Unit -> Bitmap])
  (case-lambda
    [(bmp skx sky) (bitmap-shear bmp (tan skx) (tan sky))]
    [(bmp skx sky unit) (bitmap-shear bmp (~rad skx unit) (~rad sky unit))]))

(define bitmap-shear : (-> Bitmap Real Real Bitmap)
  (lambda [bmp shx shy]
    (cond [(and (zero? shx) (zero? shy)) bmp]
          [else (bitmap_shear (bitmap-surface bmp)
                              (real->double-flonum shx)
                              (real->double-flonum shy)
                              (bitmap-density bmp))])))

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
         (bitmap-resize bmp (bitmap-intrinsic-width refer) (bitmap-intrinsic-height refer))
         (bitmap-scale bmp (/ (real->double-flonum refer)
                              (exact->inexact (min (bitmap-intrinsic-width bmp)
                                                   (bitmap-intrinsic-height bmp))))))]
    [(bmp w h)
     (bitmap-scale bmp
                   (if (zero? w) 1.0 (/ (real->double-flonum w) (exact->inexact (bitmap-intrinsic-width bmp))))
                   (if (zero? h) 1.0 (/ (real->double-flonum h) (exact->inexact (bitmap-intrinsic-height bmp)))))]))

(define bitmap-try-dsfit : (case-> [Bitmap Bitmap -> (Option Bitmap)]
                                   [Bitmap Real Real -> (Option Bitmap)]
                                   [Bitmap Bitmap Nonnegative-Real Nonnegative-Real -> (Option Bitmap)]
                                   [Bitmap Bitmap Nonnegative-Real Nonnegative-Real Geo-Insets-Datum -> (Option Bitmap)])
  (case-lambda
    [(self refer) (bitmap-dsfit self refer 1.0 1.0 0.0)]
    [(self refer wratio hratio) (bitmap-dsfit self refer wratio hratio 0.0)]
    [(self refer wratio hratio margin)
     (let-values ([(mtop mright mbottom mleft) (geo-inset-values margin)]
                  [(flwidth flheight) (bitmap-flsize refer)])
       (bitmap-dsfit self
                   (- (* flwidth  (real->double-flonum wratio)) mright mleft)
                   (- (* flheight (real->double-flonum hratio)) mtop mbottom)))]
    [(self width height)
     (let-values ([(flwidth flheight) (bitmap-flsize self)])
       (cond [(and (positive? width) (positive? height))
              (bitmap-scale self
                            (min (/ (min flwidth  (real->double-flonum width))  flwidth)
                                 (/ (min flheight (real->double-flonum height)) flheight)))]
             [(positive? width)  (bitmap-scale self (/ (min flwidth  (real->double-flonum width))  flwidth))]
             [(positive? height) (bitmap-scale self (/ (min flheight (real->double-flonum height)) flheight))]
             [else #false]))]))


(define bitmap-dsfit : (case-> [Bitmap Bitmap -> (Option Bitmap)]
                               [Bitmap Real Real -> (Option Bitmap)]
                               [Bitmap Bitmap Nonnegative-Real Nonnegative-Real -> (Option Bitmap)]
                               [Bitmap Bitmap Nonnegative-Real Nonnegative-Real Geo-Insets-Datum -> (Option Bitmap)])
  (case-lambda
    [(self refer) (or (bitmap-try-dsfit self refer 1.0 1.0 0.0) self)]
    [(self refer wratio hratio) (or (bitmap-try-dsfit self refer wratio hratio 0.0) self)]
    [(self refer wratio hratio margin) (or (bitmap-try-dsfit self refer wratio hratio margin) self)]
    [(self width height) (or (bitmap-try-dsfit self width height) self)]))

#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/unsafe/adjust.rkt")
(require "digitama/convert.rkt")

(require geofun/digitama/resize)
(require geofun/digitama/unsafe/visual/ctype)

(require digimon/metrics)

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
                               [Bitmap Real Real Real Real -> Bitmap])
  (case-lambda
    [(bmp inset) (bitmap-inset bmp inset inset inset inset)]
    [(bmp vertical horizontal) (bitmap-inset bmp vertical horizontal vertical horizontal)]
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

(define bitmap-rotate : (->* (Bitmap Real) (Boolean) Bitmap)
  (lambda [bmp theta [radian? #true]]
    (define rad (~radian theta radian?))
    
    (cond [(= rad 0.0) bmp]
          [else (bitmap_rotate (bitmap-surface bmp) rad (bitmap-density bmp))])))

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
                   (/ (real->double-flonum w) (exact->inexact (bitmap-intrinsic-width bmp)))
                   (/ (real->double-flonum h) (exact->inexact (bitmap-intrinsic-height bmp))))]))

(define bitmap-fit : (case-> [Bitmap Bitmap -> Bitmap]
                             [Bitmap Nonnegative-Real Nonnegative-Real -> Bitmap]
                             [Bitmap Bitmap Nonnegative-Real Nonnegative-Real -> Bitmap]
                             [Bitmap Bitmap Nonnegative-Real Nonnegative-Real -> Bitmap]
                             [Bitmap Bitmap Nonnegative-Real Nonnegative-Real Nonnegative-Real -> Bitmap]
                             [Bitmap Bitmap Nonnegative-Real Nonnegative-Real Nonnegative-Real Nonnegative-Real -> Bitmap])
  (case-lambda
    [(self refer) (bitmap-fit self refer 1.0 1.0)]
    [(self refer wratio hratio) (bitmap-fit self refer wratio hratio 0.0 0.0)]
    [(self refer wratio hratio pad) (bitmap-fit self refer wratio hratio pad pad)]
    [(self refer wratio hratio wpad hpad)
     (let-values ([(flwidth flheight) (bitmap-flsize refer)])
       (bitmap-fit self
                   (max (- (* flwidth  (real->double-flonum wratio)) (real->double-flonum wpad)) 0.0)
                   (max (- (* flheight (real->double-flonum hratio)) (real->double-flonum hpad)) 0.0)))]
    [(self width height)
     (let-values ([(flwidth flheight) (bitmap-flsize self)])
       (bitmap-scale self
                     (min (/ (min flwidth  (real->double-flonum width))  flwidth)
                          (/ (min flheight (real->double-flonum height)) flheight))))]))

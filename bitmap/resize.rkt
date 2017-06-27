#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/draw.rkt")
(require "digitama/resize.rkt")
(require "digitama/unsafe/resize.rkt")
(require "digitama/unsafe/source.rkt")

(define bitmap-section : (-> Bitmap Real Real Nonnegative-Real Nonnegative-Real Bitmap)
  (lambda [bmp x y width height]
    (define-values (src-width src-height) (bitmap-flsize bmp))
    (cond [(and (zero? x) (zero? y) (= width src-width) (= height src-height)) bmp]
          [else (bitmap_section (bitmap-surface bmp) (real->double-flonum x) (real->double-flonum y)
                                (flmin (real->double-flonum width) src-width) (flmin (real->double-flonum height) src-height)
                                (bitmap-density bmp))])))

(define bitmap-section/dot : (-> Bitmap Real Real Real Real Bitmap)
  (lambda [bmp x0 y0 xn yn]
    (bitmap-section bmp x0 y0 (max (- xn x0) 0) (max (- yn y0) 0))))

(define bitmap-copy : (case-> [Bitmap -> Bitmap]
                              [Bitmap Real Real Nonnegative-Real Nonnegative-Real -> Bitmap])
  (case-lambda
    [(bmp)
     (define-values (src-width src-height) (bitmap-flsize bmp))
     (bitmap_section (bitmap-surface bmp) 0.0 0.0 src-width src-height (bitmap-density bmp))]
    [(bmp x y width height)
     (define-values (src-width src-height) (bitmap-flsize bmp))
     (bitmap_section (bitmap-surface bmp) (real->double-flonum x) (real->double-flonum y)
                     (flmin (real->double-flonum width) src-width) (flmin (real->double-flonum height) src-height)
                     (bitmap-density bmp))]))

(define bitmap-enclosing-box : (->* (Bitmap) (Boolean)
                                    (Values Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [bmp [just-alpha? #true]]
    (bitmap_bounding_box* (bitmap-surface bmp) just-alpha? (bitmap-density bmp))))

(define bitmap-trim : (->* (Bitmap) (Boolean) Bitmap)
  (lambda [bmp [just-alpha? #true]]
    (define surface : Bitmap-Surface (bitmap-surface bmp))
    (define density : Flonum (bitmap-density bmp))
    (define-values (x y X Y) (bitmap_bounding_box* surface just-alpha? density))
    (bitmap_section surface x y (fl- X x) (fl- Y y) density)))

(define bitmap-inset : (case-> [Bitmap Real -> Bitmap]
                               [Bitmap Real Real -> Bitmap]
                               [Bitmap Real Real Real Real -> Bitmap])
  (case-lambda
    [(bmp inset)
     (bitmap-inset bmp inset inset inset inset)]
    [(bmp vertical horizontal)
     (bitmap-inset bmp vertical horizontal vertical horizontal)]
    [(bmp top right bottom left)
     (define-values (flleft flright) (values (real->double-flonum left) (real->double-flonum right)))
     (define-values (fltop flbottom) (values (real->double-flonum top) (real->double-flonum bottom)))
     (bitmap_section (bitmap-surface bmp) (- flleft) (- fltop)
                     (fl+ (fx->fl (send bmp get-width)) (fl+ flright flleft))
                     (fl+ (fx->fl (send bmp get-height)) (fl+ flbottom fltop))
                     (bitmap-density bmp))]))

(define-cropper bitmap-crop : (-> Bitmap Positive-Real Positive-Real Bitmap)
  (#:lambda [bmp width height left% top%]
    (define-values (W H) (values (fx->fl (send bmp get-width)) (fx->fl (send bmp get-height))))
    (define-values (w h) (values (flmin W (real->double-flonum width)) (flmin H (real->double-flonum height))))
    (bitmap_section (bitmap-surface bmp)
                    (fl* (fl- W w) left%) (fl* (fl- H h) top%) w h
                    (bitmap-density bmp)))
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
         (bitmap-resize bmp (send refer get-width) (send refer get-height))
         (bitmap-scale bmp (fl/ (real->double-flonum refer)
                                (fx->fl (fxmin (send bmp get-width)
                                               (send bmp get-height))))))]
    [(bmp w h)
     (bitmap-scale bmp
                   (fl/ (real->double-flonum w) (fx->fl (send bmp get-width)))
                   (fl/ (real->double-flonum h) (fx->fl (send bmp get-height))))]))

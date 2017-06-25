#lang typed/racket

(provide (all-defined-out))

(require "digitama/digicore.rkt")
(require "digitama/unsafe/resize.rkt")
(require "digitama/unsafe/source.rkt")
(require "constructor.rkt")

(define bitmap-section : (-> Bitmap Real Real Nonnegative-Real Nonnegative-Real Bitmap)
  (lambda [bmp x y width height]
    (define-values (src-width src-height) (values (send bmp get-width) (send bmp get-height)))
    (cond [(and (zero? x) (zero? y) (= width src-width) (= height src-height)) bmp]
          [else (bitmap_section (bitmap->surface bmp)
                                (real->double-flonum x) (real->double-flonum y)
                                (flmin (real->double-flonum width) (real->double-flonum src-width))
                                (flmin (real->double-flonum height) (real->double-flonum src-height))
                                (real->double-flonum (send bmp get-backing-scale)))])))

(define bitmap-section/dot : (-> Bitmap Real Real Real Real Bitmap)
  (lambda [bmp x0 y0 xn yn]
    (bitmap-section bmp x0 y0 (max (- xn x0) 0) (max (- yn y0) 0))))

(define bitmap-copy : (case-> [Bitmap -> Bitmap]
                              [Bitmap Real Real Nonnegative-Real Nonnegative-Real -> Bitmap])
  (case-lambda
    [(bmp)
     (bitmap_section (bitmap->surface bmp) 0.0 0.0
                     (fx->fl (send bmp get-width)) (fx->fl (send bmp get-height))
                     (real->double-flonum (send bmp get-backing-scale)))]
    [(bmp x y width height)
     (bitmap_section (bitmap->surface bmp)
                     (real->double-flonum x) (real->double-flonum y)
                     (flmin (real->double-flonum width) (fx->fl (send bmp get-width)))
                     (flmin (real->double-flonum height) (fx->fl (send bmp get-height)))
                     (real->double-flonum (send bmp get-backing-scale)))]))

(define bitmap-enclosing-box : (->* (Bitmap) (Boolean)
                                    (Values Flonum Flonum Flonum Flonum))
  (lambda [bmp [just-alpha? #true]]
    (define density : Nonnegative-Flonum (real->double-flonum (send bmp get-backing-scale)))
    (define-values (x y X Y) (bitmap_bounding_box (bitmap->surface bmp) just-alpha?))
    (values (fl/ (fx->fl x) density) (fl/ (fx->fl y) density)
            (fl/ (fx->fl X) density) (fl/ (fx->fl Y) density))))

(define bitmap-trim : (->* (Bitmap) (Boolean) Bitmap)
  (lambda [bmp [just-alpha? #true]]
    (define density : Flonum (real->double-flonum (send bmp get-backing-scale)))
    (define-values (x y X Y) (bitmap_bounding_box (bitmap->surface bmp) just-alpha?))
    (bitmap_section (bitmap->surface bmp)
                    (fl/ (fx->fl x) density) (fl/ (fx->fl y) density)
                    (fl/ (fx->fl (fx- (fx- X 1) x)) density) (fl/ (fx->fl (fx- (fx- Y 1) y)) density)
                    density)))

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
     (bitmap_section (bitmap->surface bmp) (- flleft) (- fltop)
                     (fl+ (fx->fl (send bmp get-width)) (fl+ flright flleft))
                     (fl+ (fx->fl (send bmp get-height)) (fl+ flbottom fltop))
                     (real->double-flonum (send bmp get-backing-scale)))]))

(define bitmap-crop : (-> Bitmap Positive-Real Positive-Real Flonum Flonum Bitmap)
  (lambda [bmp width height left% top%]
    (define-values (W H) (values (fx->fl (send bmp get-width)) (fx->fl (send bmp get-height))))
    (define-values (w h) (values (flmin W (real->double-flonum width)) (flmin H (real->double-flonum height))))
    (bitmap_section (bitmap->surface bmp)
                    (fl* (fl- W w) left%) (fl* (fl- H h) top%) w h
                    (real->double-flonum (send bmp get-backing-scale)))))

(define bitmap-lt-crop : (-> Bitmap Positive-Real Positive-Real Bitmap) (λ [bmp w h] (bitmap-crop bmp w h 0.0 0.0)))
(define bitmap-lc-crop : (-> Bitmap Positive-Real Positive-Real Bitmap) (λ [bmp w h] (bitmap-crop bmp w h 0.0 0.5)))
(define bitmap-lb-crop : (-> Bitmap Positive-Real Positive-Real Bitmap) (λ [bmp w h] (bitmap-crop bmp w h 0.0 1.0)))
(define bitmap-ct-crop : (-> Bitmap Positive-Real Positive-Real Bitmap) (λ [bmp w h] (bitmap-crop bmp w h 0.5 0.0)))
(define bitmap-cc-crop : (-> Bitmap Positive-Real Positive-Real Bitmap) (λ [bmp w h] (bitmap-crop bmp w h 0.5 0.5)))
(define bitmap-cb-crop : (-> Bitmap Positive-Real Positive-Real Bitmap) (λ [bmp w h] (bitmap-crop bmp w h 0.5 1.0)))
(define bitmap-rt-crop : (-> Bitmap Positive-Real Positive-Real Bitmap) (λ [bmp w h] (bitmap-crop bmp w h 1.0 0.0)))
(define bitmap-rc-crop : (-> Bitmap Positive-Real Positive-Real Bitmap) (λ [bmp w h] (bitmap-crop bmp w h 1.0 0.5)))
(define bitmap-rb-crop : (-> Bitmap Positive-Real Positive-Real Bitmap) (λ [bmp w h] (bitmap-crop bmp w h 1.0 1.0)))

(define bitmap-scale : (->* (Bitmap Real) (Real) Bitmap)
  (case-lambda
    [(bmp scale)
     (if (= scale 1.0) bmp (bitmap-scale bmp scale scale))]
    [(bmp scale-x scale-y)
     (cond [(and (= scale-x 1.0) (= scale-y 1.0)) bmp]
           [else (bitmap_scale (bitmap->surface bmp)
                               (real->double-flonum scale-x)
                               (real->double-flonum scale-y)
                               (real->double-flonum (send bmp get-backing-scale)))])]))

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

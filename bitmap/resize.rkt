#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/bitmap.rkt")
(require "constructor.rkt")

(define bitmap-section : (-> Bitmap Real Real Nonnegative-Real Nonnegative-Real Bitmap)
  (lambda [bmp left top width height]
    (define-values (src-width src-height) (values (send bmp get-width) (send bmp get-height)))
    (cond [(and (zero? left) (zero? top) (= width src-width) (= height src-height)) bmp]
          [else (bitmap-copy bmp left top width height)])))

(define bitmap-section/dot : (-> Bitmap Real Real Real Real Bitmap)
  (lambda [bmp left top right bottom]
    (bitmap-section bmp left top (max (- right left) 0) (max (- bottom top) 0))))

(define bitmap-copy : (case-> [Bitmap -> Bitmap]
                              [Bitmap Real Real Nonnegative-Real Nonnegative-Real -> Bitmap])
  (case-lambda
    [(bmp) (bitmap-copy bmp 0 0 (send bmp get-width) (send bmp get-height))]
    [(bmp left top width height)
     (define clone : Bitmap (bitmap-blank width height (send bmp get-backing-scale)))
     (define dc : (Instance Bitmap-DC%) (send clone make-dc))
     (send dc set-smoothing 'aligned)
     (send dc draw-bitmap-section bmp 0 0 left top width height)
     clone]))

(define bitmap-inset : (case-> [Bitmap Real -> Bitmap]
                               [Bitmap Real Real -> Bitmap]
                               [Bitmap Real Real Real Real -> Bitmap])
  (case-lambda
    [(bmp inset) (bitmap-inset bmp inset inset inset inset)]
    [(bmp horizontal vertical) (bitmap-inset bmp horizontal vertical horizontal vertical)]
    [(bmp left top right bottom)
     (bitmap-section/dot bmp (- left) (- top) (+ (send bmp get-width) right) (+ (send bmp get-height) bottom))]))

(define bitmap-crop : (-> Bitmap Positive-Real Positive-Real Real Real Bitmap)
  (lambda [bmp width height left% top%]
    (define-values (w h) (values (send bmp get-width) (send bmp get-height)))
    (define-values (x y) (values (* (- width w) left%) (* (- height h) top%)))
    (bitmap-inset bmp x y (- width w x) (- height h y))))

(define bitmap-lt-crop : (-> Bitmap Positive-Real Positive-Real Bitmap) (λ [bmp w h] (bitmap-crop bmp w h 0 0)))
(define bitmap-lc-crop : (-> Bitmap Positive-Real Positive-Real Bitmap) (λ [bmp w h] (bitmap-crop bmp w h 0 1/2)))
(define bitmap-lb-crop : (-> Bitmap Positive-Real Positive-Real Bitmap) (λ [bmp w h] (bitmap-crop bmp w h 0 1)))
(define bitmap-ct-crop : (-> Bitmap Positive-Real Positive-Real Bitmap) (λ [bmp w h] (bitmap-crop bmp w h 1/2 0)))
(define bitmap-cc-crop : (-> Bitmap Positive-Real Positive-Real Bitmap) (λ [bmp w h] (bitmap-crop bmp w h 1/2 1/2)))
(define bitmap-cb-crop : (-> Bitmap Positive-Real Positive-Real Bitmap) (λ [bmp w h] (bitmap-crop bmp w h 1/2 1)))
(define bitmap-rt-crop : (-> Bitmap Positive-Real Positive-Real Bitmap) (λ [bmp w h] (bitmap-crop bmp w h 1 0)))
(define bitmap-rc-crop : (-> Bitmap Positive-Real Positive-Real Bitmap) (λ [bmp w h] (bitmap-crop bmp w h 1 1/2)))
(define bitmap-rb-crop : (-> Bitmap Positive-Real Positive-Real Bitmap) (λ [bmp w h] (bitmap-crop bmp w h 1 1)))

(define bitmap-scale : (->* (Bitmap Real) (Real) Bitmap)
  (case-lambda
    [(bmp scale)
     (if (= scale 1.0) bmp (bitmap-scale bmp scale scale))]
    [(bmp scale-x scale-y)
     (cond [(and (= scale-x 1.0) (= scale-y 1.0)) bmp]
           [else (let ([width : Nonnegative-Real (* (send bmp get-width) (abs scale-x))]
                       [height : Nonnegative-Real (* (send bmp get-height) (abs scale-y))])
                   (define scaled : Bitmap (bitmap-blank width height (send bmp get-backing-scale)))
                   (define dc : (Instance Bitmap-DC%) (send scaled make-dc))
                   (send dc set-smoothing 'aligned)
                   (send dc set-scale scale-x scale-y)
                   (send dc draw-bitmap bmp 0 0)
                   scaled)])]))

(define bitmap-resize : (case-> [Bitmap (U Bitmap Nonnegative-Real) -> Bitmap]
                                [Bitmap Nonnegative-Real Nonnegative-Real -> Bitmap])
  (case-lambda
    [(bmp refer)
     (cond [(real? refer) (bitmap-scale bmp (/ refer (min (send bmp get-width) (send bmp get-height))))]
           [else (bitmap-resize bmp (send refer get-width) (send refer get-height))])]
    [(bmp w h)
     (bitmap-scale bmp (/ w (send bmp get-width)) (/ h (send bmp get-height)))]))

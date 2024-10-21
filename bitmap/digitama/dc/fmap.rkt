#lang typed/racket/base

(provide (all-defined-out))
(provide XYWH->ARGB XYWH->ARGB* ARGB-Step)

(require digimon/metrics)
(require geofun/digitama/base)

(require "../convert.rkt")
(require "../unsafe/image.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-rectangular : (-> Real Real XYWH->ARGB [#:density Positive-Flonum] Bitmap)
  (lambda [width height λargb #:density [density (default-bitmap-density)]]
    (define-values (w h) (~size width height))
    (λbitmap w h density #false λargb)))

(define bitmap-rectangular* : (All (t) (-> Real Real (XYWH->ARGB* t) t [#:density Positive-Flonum] (Values Bitmap t)))
  (lambda [width height λargb initial #:density [density (default-bitmap-density)]]
    (define-values (w h) (~size width height))
    (λbitmap* w h density #false λargb initial)))

(define bitmap-irregular : (All (t) (-> Real Real (ARGB-Step t) t [#:density Positive-Flonum] Bitmap))
  (lambda [width height λargb initial #:density [density (default-bitmap-density)]]
    (define-values (w h) (~size width height))
    (define-values (bmp _) (bitmap-irregular* w h λargb initial #:density density))
    bmp))

(define bitmap-irregular* : (All (t) (-> Real Real (ARGB-Step t) t [#:density Positive-Flonum] (Values Bitmap t)))
  (lambda [width height λargb initial #:density [density (default-bitmap-density)]]
    (define-values (w h) (~size width height))
    (λbitmap_step w h density #false λargb initial)))

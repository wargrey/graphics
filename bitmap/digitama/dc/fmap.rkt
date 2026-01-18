#lang typed/racket/base

(provide (all-defined-out))
(provide XYWH->ARGB XYWH->ARGB* ARGB-Step)

(require digimon/measure)
(require geofun/digitama/base)

(require "../self.rkt")
(require "../unsafe/image.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-rectangular : (-> Real-Length Length+% XYWH->ARGB [#:density Positive-Flonum] Bitmap)
  (lambda [width height λargb #:density [density (default-bitmap-density)]]
    (define-values (w h) (~extent width height))
    (λbitmap w h density λargb)))

(define bitmap-rectangular* : (All (t) (-> Real-Length Length+% (XYWH->ARGB* t) t [#:density Positive-Flonum] (Values Bitmap t)))
  (lambda [width height λargb initial #:density [density (default-bitmap-density)]]
    (define-values (w h) (~extent width height))
    (λbitmap* w h density λargb initial)))

(define bitmap-irregular : (All (t) (-> Real-Length Length+% (ARGB-Step t) t [#:density Positive-Flonum] Bitmap))
  (lambda [width height λargb initial #:density [density (default-bitmap-density)]]
    (define-values (w h) (~extent width height))
    (define-values (bmp _) (bitmap-irregular* w h λargb initial #:density density))
    bmp))

(define bitmap-irregular* : (All (t) (-> Real-Length Length+% (ARGB-Step t) t [#:density Positive-Flonum] (Values Bitmap t)))
  (lambda [width height λargb initial #:density [density (default-bitmap-density)]]
    (define-values (w h) (~extent width height))
    (λbitmap_step w h density λargb initial)))

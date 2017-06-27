#lang typed/racket/base

(provide (all-defined-out))

(require "../draw.rkt")
(require "source.rkt")
(require "require.rkt")

(module unsafe racket/base
  (provide (all-defined-out) cairo-image-size)
  
  (require "pangocairo.rkt")
  (require "paint.rkt")
  
  (define (bitmap_composite operator sfc1 x1 y1 sfc2 x2 y2 density)
    (define-values (w1 h1) (cairo-image-size sfc1 density))
    (define-values (w2 h2) (cairo-image-size sfc2 density))
    (define-values (dx dy) (values (unsafe-fl- x1 x2) (unsafe-fl- y1 y2)))
    (define-values (dx1 dy1) (values (unsafe-flmax (unsafe-fl- 0.0 dx) 0.0) (unsafe-flmax (unsafe-fl- 0.0 dy) 0.0)))
    (define-values (dx2 dy2) (values (unsafe-flmax dx 0.0) (unsafe-flmax dy 0.0)))
    (define-values (img cr w h)
      (make-cairo-image (unsafe-flmax (unsafe-fl+ dx1 w1) (unsafe-fl+ dx2 w2))
                        (unsafe-flmax (unsafe-fl+ dy1 h1) (unsafe-fl+ dy2 h2))
                        density #true))
    (cairo_save cr)
    (cairo-composite cr sfc1 dx1 dy1 w1 h1 CAIRO_FILTER_BILINEAR CAIRO_OPERATOR_SOURCE density)
    (cairo_restore cr)
    (cairo-composite cr sfc2 dx2 dy2 w2 h2 CAIRO_FILTER_BILINEAR operator density)
    (cairo_destroy cr)
    img)

  (define bitmap_pin
    (lambda [x1% y1% x2% y2% sfc1 sfc2 density]
      (define-values (w1 h1) (cairo-image-size sfc1 density))
      (define-values (w2 h2) (cairo-image-size sfc2 density))
      (bitmap_composite CAIRO_OPERATOR_OVER sfc1
                        (unsafe-fl- (unsafe-fl* x1% w1) (unsafe-fl* x2% w2))
                        (unsafe-fl- (unsafe-fl* y1% h1) (unsafe-fl* y2% h2))
                        sfc2 0.0 0.0 density)))

  (define bitmap_pin*
    (lambda [x1% y1% x2% y2% sfc1 sfc2 density]
      (define-values (w1 h1) (cairo-image-size sfc1 density))
      (define-values (w2 h2) (cairo-image-size sfc2 density))
      (bitmap_composite CAIRO_OPERATOR_OVER sfc1
                        (unsafe-fl- (unsafe-fl* x1% w1) (unsafe-fl* x2% w2))
                        (unsafe-fl- (unsafe-fl* y1% h1) (unsafe-fl* y2% h2))
                        sfc2 0.0 0.0 density))))

(unsafe/require/provide
 (submod "." unsafe)
 [bitmap_composite (-> Integer Bitmap-Surface Flonum Flonum Bitmap-Surface Flonum Flonum Flonum Bitmap)]
 [bitmap_pin (-> Flonum Flonum Flonum Flonum Bitmap-Surface Bitmap-Surface Flonum Bitmap)])

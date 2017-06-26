#lang typed/racket/base

(provide (all-defined-out))

(require "../draw.rkt")
(require "source.rkt")
(require "require.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require "pangocairo.rkt")
  (require "paint.rkt")

  (define (bitmap_composite operator sfc1 x1 y1 sfc2 x2 y2 density)
    (define-values (width1 height1) (cairo-image-size sfc1 density))
    (define-values (width2 height2) (cairo-image-size sfc2 density))
    (define-values (dx dy) (values (unsafe-fl- x1 x2) (unsafe-fl- y1 y2)))
    (define-values (dx1 dy1) (values (unsafe-flmax (unsafe-fl- 0.0 dx) 0.0) (unsafe-flmax (unsafe-fl- 0.0 dy) 0.0)))
    (define-values (dx2 dy2) (values (unsafe-flmax dx 0.0) (unsafe-flmax dy 0.0)))
    (define-values (img cr w h)
      (make-cairo-image (unsafe-flmax (unsafe-fl+ dx1 width1) (unsafe-fl+ dx2 width2))
                        (unsafe-flmax (unsafe-fl+ dy1 height1) (unsafe-fl+ dy2 height2))
                        density #true))
    (cairo_save cr)
    (cairo-composite cr sfc1 dx1 dy1 width1 height1 CAIRO_FILTER_BILINEAR CAIRO_OPERATOR_SOURCE density)
    (cairo_restore cr)
    (cairo-composite cr sfc2 dx2 dy2 width2 height2 CAIRO_FILTER_BILINEAR operator density)
    (cairo_destroy cr)
    img))

(unsafe/require/provide
 (submod "." unsafe)
 [bitmap_composite (-> Integer Bitmap-Surface Flonum Flonum Bitmap-Surface Flonum Flonum Flonum Bitmap)])

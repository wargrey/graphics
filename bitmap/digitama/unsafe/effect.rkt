#lang typed/racket/base

(provide (all-defined-out))

(require "convert.rkt")
(require "require.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require "pangocairo.rkt")
  (require (submod "pixman.rkt" unsafe))
  (require (submod "convert.rkt" unsafe))

  (define (bitmap_cellophane src alpha density)
    (define-values (flwidth flheight) (cairo-surface-size src density))
    (define-values (img cr) (make-cairo-image flwidth flheight density #false))
    (cairo_set_source_surface cr src 0.0 0.0)
    (cairo_paint_with_alpha cr alpha)
    (cairo_destroy cr)
    img)

  (define (bitmap_grayscale src rgb->gray density)
    (define-values (flwidth flheight) (cairo-surface-size src density))
    (define-values (img cr) (make-cairo-image flwidth flheight density #false))
    (define surface (cairo_get_target cr))
    (define-values (data total) (cairo-surface-data src))
    (define-values (pixels _) (cairo-surface-data surface))
    (memcpy pixels data total _byte)
    (let grayscale ([idx 0])
      (when (unsafe-fx< idx total)
        (define-values (r g b) (pixels-get-rgb-bytes data idx))
        (define gray (rgb->gray r g b))
        (pixels-set-rgb-bytes pixels idx gray gray gray)
        (grayscale (unsafe-fx+ idx 4))))
    (cairo_surface_mark_dirty surface)
    (cairo_destroy cr)
    img))

(unsafe/require/provide
 (submod "." unsafe)
 [bitmap_cellophane (-> Bitmap-Surface Flonum Flonum Bitmap)]
 [bitmap_grayscale (-> Bitmap-Surface (-> Byte Byte Byte Integer) Flonum Bitmap)])

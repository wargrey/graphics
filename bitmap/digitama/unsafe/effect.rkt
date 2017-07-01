#lang typed/racket/base

(provide (all-defined-out))

(require "../draw.rkt")
(require "draw.rkt")
(require "require.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require "pangocairo.rkt")

  (define (bitmap_cellophane src alpha density)
    (define-values (flwidth flheight) (cairo-image-size src density))
    (define-values (img cr) (make-cairo-image flwidth flheight density #false))
    (cairo_set_source_surface cr src 0.0 0.0)
    (cairo_paint_with_alpha cr alpha)
    (cairo_destroy cr)
    img)

  (define (bitmap_grayscale src rgb->gray density)
    (define-values (flwidth flheight) (cairo-image-size src density))
    (define-values (img cr) (make-cairo-image flwidth flheight density #false))
    (define surface (cairo_get_target cr))
    (define-values (data total) (cairo-surface-data src))
    (define-values (buffer _) (cairo-surface-data surface))
    (memcpy buffer data total _byte)
    (let grayscale ([idx 0])
      (when (unsafe-fx< idx total)
        (define-values (r g b) (values (unsafe-fx+ idx R) (unsafe-fx+ idx G) (unsafe-fx+ idx B)))
        (define gray (rgb->gray (unsafe-bytes-ref data r) (unsafe-bytes-ref data g) (unsafe-bytes-ref data b)))
        (unsafe-bytes-set! buffer r gray)
        (unsafe-bytes-set! buffer g gray)
        (unsafe-bytes-set! buffer b gray)
        (grayscale (unsafe-fx+ idx 4))))
    (cairo_surface_mark_dirty surface)
    (cairo_destroy cr)
    img))

(unsafe/require/provide
 (submod "." unsafe)
 [bitmap_cellophane (-> Bitmap-Surface Flonum Flonum Bitmap)]
 [bitmap_grayscale (-> Bitmap-Surface (-> Byte Byte Byte Integer) Flonum Bitmap)])

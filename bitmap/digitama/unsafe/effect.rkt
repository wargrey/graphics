#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require geofun/digitama/unsafe/visual/ctype)
(require "../convert.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require racket/draw/unsafe/cairo)
  (require racket/unsafe/ops)
  (require ffi/unsafe)

  (require "../convert.rkt")
  (require "pixman.rkt")
  (require (submod "bitmap.rkt" unsafe))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (bitmap_cellophane src alpha density)
    (define-values (flwidth flheight) (bitmap-surface-rendered-size src density))
    (define-values (img cr) (create-argb-bitmap flwidth flheight density #false))
    (cairo_set_source_surface cr src 0.0 0.0)
    (cairo_paint_with_alpha cr alpha)
    img)

  (define (bitmap_grayscale src rgb->gray density)
    (define-values (flwidth flheight) (bitmap-surface-rendered-size src density))
    (define-values (img cr) (create-argb-bitmap flwidth flheight density #false))
    (define surface (cairo_get_target cr))
    (define-values (data total) (bitmap-surface-data src))
    (define-values (pixels _) (bitmap-surface-data surface))

    (cairo_surface_flush surface)

    ; this is more efficient than read/write ARGB
    (memcpy pixels data total _byte)
    (let grayscale ([idx 0])
      (when (unsafe-fx< idx total)
        (define-values (r g b) (pixels-get-rgb-bytes data idx))
        (define gray (rgb->gray r g b))
        (pixels-set-rgb-bytes pixels idx gray gray gray)
        (grayscale (unsafe-fx+ idx 4))))

    (cairo_surface_mark_dirty surface)
    img))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [bitmap_cellophane (-> Bitmap-Surface Flonum Flonum Bitmap)]
 [bitmap_grayscale (-> Bitmap-Surface (-> Byte Byte Byte Integer) Flonum Bitmap)])

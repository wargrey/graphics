#lang typed/racket/base

(provide (all-defined-out))

(require "../draw.rkt")
(require "source.rkt")
(require "require.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require "pangocairo.rkt")

  (define (bitmap_cellophane src alpha density)
    (define flwidth (unsafe-fl/ (unsafe-fx->fl (cairo_image_surface_get_width src)) density))
    (define flheight (unsafe-fl/ (unsafe-fx->fl (cairo_image_surface_get_height src)) density))
    (define-values (img cr w h) (make-cairo-image flwidth flheight density))
    (define 1/density (unsafe-fl/ 1.0 density))
    (cairo_scale cr 1/density 1/density) ; order matters
    (cairo_set_source_surface cr src 0.0 0.0)
    (cairo_paint_with_alpha cr alpha)
    (cairo_destroy cr)
    img)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  )

(define-type XYWH->ARGB (-> Nonnegative-Fixnum Nonnegative-Fixnum Positive-Fixnum Positive-Fixnum (Values Real Real Real Real)))

(unsafe/require/provide
 (submod "." unsafe)
 [bitmap_cellophane (-> Bitmap-Surface Real Flonum Bitmap)])

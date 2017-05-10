#lang typed/racket/base

(provide (all-defined-out))
(provide Font-Description font-description?)
(provide Bitmap-Surface bitmap-surface?)
(provide Bitmap-Pattern bitmap-pattern?)

(require typed/racket/unsafe)

(define-type Bitmap-Source (U Bitmap-Surface Bitmap-Pattern FlVector))

(module unsafe racket/base
  (provide (all-defined-out) cpointer?)
  
  (require "ffi.rkt")

  (require racket/unsafe/ops)

  (define (font-description? v) (cpointer*? v 'PangoFontDescription))
  
  (define (bitmap-surface? v) (cpointer*? v 'cairo_surface_t))
  (define (bitmap-pattern? v) (cpointer*? v 'cairo_pattern_t))
  
  (define (bitmap-linear-gradient x0 y0 x1 y1 stops)
    (define gradient (cairo_pattern_create_linear x0 y0 x1 y1))
    (for ([stop (in-list stops)])
      (gradient-add-color-stop gradient (unsafe-car stop) (unsafe-cdr stop)))
    gradient)
  
  (define (bitmap-radial-gradient x0 y0 r0 x1 y1 r1 stops)
    (define gradient (cairo_pattern_create_radial x0 y0 r0 x1 y1 r1))
    (for ([stop (in-list stops)])
      (gradient-add-color-stop gradient (unsafe-car stop) (unsafe-cdr stop)))
    gradient)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (cpointer*? v tag)
    (and (cpointer? v)
         (eq? (cpointer-tag v) tag)))

  (define (gradient-add-color-stop gradient position rgba)
    (cairo_pattern_add_color_stop_rgba gradient position
                                       (unsafe-flvector-ref rgba 0)
                                       (unsafe-flvector-ref rgba 1)
                                       (unsafe-flvector-ref rgba 2)
                                       (unsafe-flvector-ref rgba 3))))

(unsafe-require/typed
 (submod "." unsafe)
 [#:opaque Font-Description font-description?]
 [#:opaque Bitmap-Surface bitmap-surface?]
 [#:opaque Bitmap-Pattern bitmap-pattern?])

(require/typed/provide
 (submod "." unsafe)
 [bitmap-linear-gradient (-> Real Real Real Real (Listof (Pairof Real FlVector)) Bitmap-Pattern)]
 [bitmap-radial-gradient (-> Real Real Real Real Real Real (Listof (Pairof Real FlVector)) Bitmap-Pattern)])

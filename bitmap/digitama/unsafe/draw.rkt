#lang typed/racket/base

(provide (all-defined-out))

(require "../draw.rkt")
(require "require.rkt")

(define-type Bitmap-Source (U Bitmap-Surface Bitmap-Pattern FlRGBA))
(define-type Bitmap-FlSize*
  (case-> [Bitmap Positive-Flonum Positive-Flonum -> (Values Positive-Flonum Positive-Flonum)]
          [Bitmap Nonnegative-Flonum Nonnegative-Flonum -> (Values Nonnegative-Flonum Nonnegative-Flonum)]))

(module unsafe racket/base
  (provide (all-defined-out) the-surface)
  
  (require "pangocairo.rkt")

  (define (font-description? v) (cpointer*? v 'PangoFontDescription))
  (define (bitmap-surface? v) (cpointer*? v 'cairo_surface_t))
  (define (bitmap-pattern? v) (cpointer*? v 'cairo_pattern_t))

  (define (bitmap%? v) (is-a? v bitmap%))
  (define (bitmap-surface bmp) (send bmp get-handle))
  (define (bitmap-density bmp) (real->double-flonum (send bmp get-backing-scale)))
  (define (bitmap-flsize bmp) (values (unsafe-fx->fl (send bmp get-width)) (unsafe-fx->fl (send bmp get-height))))
  
  (define (bitmap-flsize* bmp w% h%)
    (define-values (w h) (bitmap-flsize bmp))
    (values (unsafe-fl* w w%) (unsafe-fl* h h%)))

  (define (bitmap-intrinsic-flsize bmp)
    (define density (bitmap-density bmp))
    (bitmap-flsize* bmp density density))
  
  (define (bitmap-linear-gradient-pattern x0 y0 x1 y1 stops)
    (define gradient (cairo_pattern_create_linear x0 y0 x1 y1))
    (for ([stop (in-list stops)])
      (gradient-add-color-stop gradient (unsafe-car stop) (unsafe-cdr stop)))
    gradient)
  
  (define (bitmap-radial-gradient-pattern x0 y0 r0 x1 y1 r1 stops)
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
                                       (unsafe-struct*-ref rgba 0)
                                       (unsafe-struct*-ref rgba 1)
                                       (unsafe-struct*-ref rgba 2)
                                       (unsafe-struct*-ref rgba 3))))

(unsafe/require/provide
 (submod "." unsafe)
 [#:opaque
  [Font-Description font-description?]
  [Bitmap-Surface bitmap-surface?]
  [Bitmap-Pattern bitmap-pattern?]]
 [the-surface Bitmap-Surface]
 [bitmap%? (-> Any Boolean : Bitmap)]
 [bitmap-surface (-> Bitmap Bitmap-Surface)]
 [bitmap-density (-> Bitmap Positive-Flonum)]
 [bitmap-intrinsic-flsize (-> Bitmap (Values Positive-Flonum Positive-Flonum))]
 [bitmap-flsize (-> Bitmap (Values Positive-Flonum Positive-Flonum))]
 [bitmap-flsize* Bitmap-FlSize*]
 [bitmap-linear-gradient-pattern (-> Real Real Real Real (Listof (Pairof Real FlRGBA)) Bitmap-Pattern)]
 [bitmap-radial-gradient-pattern (-> Real Real Real Real Real Real (Listof (Pairof Real FlRGBA)) Bitmap-Pattern)])

#lang typed/racket/base

(provide (all-defined-out) bitmap%?)
(provide Font-Description font-description?)
(provide Bitmap-Surface bitmap-surface?)
(provide Bitmap-Pattern bitmap-pattern?)

(require "../draw.rkt")

(require typed/racket/unsafe)

(define-type Paint-Property paint-property)
(define-type FlColor flcolor)
(define-type FlRGBA rgba)
(define-type Bitmap-Source (U Bitmap-Surface Bitmap-Pattern FlRGBA))
(define-type Color (U Symbol Integer FlColor))

(struct paint-property () #:transparent)
(struct flcolor () #:transparent)
(struct rgba flcolor ([red : Flonum] [green : Flonum] [blue : Flonum] [alpha : Flonum]) #:transparent)

(module unsafe racket/base
  (provide (all-defined-out) cpointer?)
  
  (require "pangocairo.rkt")

  (require racket/unsafe/ops)

  (define (bitmap%? v) (is-a? v bitmap%))
  (define (font-description? v) (cpointer*? v 'PangoFontDescription))
  (define (bitmap-surface? v) (cpointer*? v 'cairo_surface_t))
  (define (bitmap-pattern? v) (cpointer*? v 'cairo_pattern_t))
  
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

(unsafe-require/typed
 (submod "." unsafe)
 [#:opaque Font-Description font-description?]
 [#:opaque Bitmap-Surface bitmap-surface?]
 [#:opaque Bitmap-Pattern bitmap-pattern?]
 [bitmap%? (-> Any Boolean : Bitmap)])

(require/typed/provide
 (submod "." unsafe)
 [bitmap-linear-gradient-pattern (-> Real Real Real Real (Listof (Pairof Real FlRGBA)) Bitmap-Pattern)]
 [bitmap-radial-gradient-pattern (-> Real Real Real Real Real Real (Listof (Pairof Real FlRGBA)) Bitmap-Pattern)])

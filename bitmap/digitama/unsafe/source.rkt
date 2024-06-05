#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "../base.rkt")
(require "convert.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(define-type Bitmap-Source (U Bitmap-Surface Bitmap-Pattern FlRGBA))
(define-type Bitmap-FlSize*
  (case-> [Bitmap Positive-Flonum Positive-Flonum -> (Values Positive-Flonum Positive-Flonum)]
          [Bitmap Nonnegative-Flonum Nonnegative-Flonum -> (Values Nonnegative-Flonum Nonnegative-Flonum)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(module unsafe racket/base
  (provide (all-defined-out) the-surface)
  (provide (rename-out [cpointer? font-description?]))
  (provide (rename-out [cpointer? bitmap-pattern?]))
  
  (require "pangocairo.rkt")
  (require "surface/image.rkt")

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
  (define (gradient-add-color-stop gradient position rgba)
    (cairo_pattern_add_color_stop_rgba gradient position
                                       (unsafe-struct*-ref rgba 0)
                                       (unsafe-struct*-ref rgba 1)
                                       (unsafe-struct*-ref rgba 2)
                                       (unsafe-struct*-ref rgba 3))))

(unsafe-require/typed/provide
 (submod "." unsafe)
 [#:opaque Bitmap-Pattern bitmap-pattern?]
 [the-surface Bitmap-Surface]
 [bitmap-linear-gradient-pattern (-> Real Real Real Real (Listof (Pairof Real FlRGBA)) Bitmap-Pattern)]
 [bitmap-radial-gradient-pattern (-> Real Real Real Real Real Real (Listof (Pairof Real FlRGBA)) Bitmap-Pattern)])

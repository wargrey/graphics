#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "../base.rkt")
(require "visual/ctype.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(define-type Fill-Source (U Cairo-Surface Fill-Pattern FlRGBA))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module unsafe racket/base
  (provide (all-defined-out))
  (provide (rename-out [cpointer? font-description?]))
  (provide (rename-out [cpointer? fill-pattern?]))
  
  (require "pangocairo.rkt")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (geo-linear-gradient-pattern x0 y0 x1 y1 stops)
    (define gradient (cairo_pattern_create_linear x0 y0 x1 y1))
    (for ([stop (in-list stops)])
      (gradient-add-color-stop gradient (unsafe-car stop) (unsafe-cdr stop)))
    gradient)
  
  (define (geo-radial-gradient-pattern x0 y0 r0 x1 y1 r1 stops)
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
 [#:opaque Fill-Pattern fill-pattern?]
 [geo-linear-gradient-pattern (-> Real Real Real Real (Listof (Pairof Real FlRGBA)) Fill-Pattern)]
 [geo-radial-gradient-pattern (-> Real Real Real Real Real Real (Listof (Pairof Real FlRGBA)) Fill-Pattern)])

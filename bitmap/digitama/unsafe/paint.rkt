#lang racket/base

(provide (all-defined-out))

(require "pangocairo.rkt")
(require "../paint.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cairo-set-stroke
  (lambda [cr stroke]
    (cairo-set-source cr (unsafe-struct-ref stroke 0))
    (cairo_set_line_width cr (unsafe-struct-ref stroke 1))
    (cairo_set_line_cap cr (linecap->integer (unsafe-struct-ref stroke 2)))
    (cairo_set_line_join cr (linejoin->integer (unsafe-struct-ref stroke 3)))
    (let ([ml (unsafe-struct-ref stroke 4)]) (unless (nan? ml) (cairo_set_miter_limit cr ml)))
    (cairo_set_dash cr (unsafe-struct-ref stroke 5) (unsafe-struct-ref stroke 6))))

(define cairo-render-with-stroke
  (lambda [cr border]
    (cairo-set-stroke cr border)
    (cairo_stroke_preserve cr)))

(define cairo-render-with-background
  (lambda [cr background]
    (cairo-set-source cr background)
    (cairo_fill_preserve cr)))

(define cairo-render
  (lambda [cr border background]
    (unless (not background)
      (cairo-render-with-background cr background))
    (unless (not border)
      (cairo-render-with-stroke cr border))))

(define cairo-composite
  (lambda [cr src dest-x dest-y dest-width dest-height filter operator density restore?]
    (define 1/density (unsafe-fl/ 1.0 density))

    (when restore? (cairo_save cr))
    (cairo_translate cr dest-x dest-y)
    (cairo_rectangle cr 0.0 0.0 dest-width dest-height)
    (cairo_scale cr 1/density 1/density) ; order matters
    (cairo_set_source_surface cr src 0.0 0.0)
    (cairo_pattern_set_filter (cairo_get_source cr) filter)
    (cairo_set_operator cr operator)
    (cairo_fill cr)
    (when restore? (cairo_restore cr))))

(define cairo-mask
  (lambda [cr src dest-x dest-y dest-width dest-height density]
    (define 1/density (unsafe-fl/ 1.0 density))

    (cairo_save cr)
    (cairo_translate cr dest-x dest-y)
    (cairo_scale cr 1/density 1/density) ; order matters
    (cairo_mask_surface cr src 0.0 0.0)
    (cairo_restore cr)))

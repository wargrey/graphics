#lang racket/base

(provide (all-defined-out))

(require "pangocairo.rkt")
(require "../paint.rkt")

(define cairo-set-stroke
  (lambda [cr stroke]
    (cairo-set-source cr (unsafe-struct-ref stroke 0))
    (cairo_set_line_width cr (unsafe-struct-ref stroke 1))
    (cairo_set_line_cap cr (linecap->integer (unsafe-struct-ref stroke 2)))
    (cairo_set_line_join cr (linejoin->integer (unsafe-struct-ref stroke 3)))
    (let ([ml (unsafe-struct-ref stroke 4)]) (unless (nan? ml) (cairo_set_miter_limit cr ml)))
    (cairo_set_dash cr (unsafe-struct-ref stroke 5) (unsafe-struct-ref stroke 6))))

(define cairo-render
  (lambda [cr border background]
    (unless (not background)
      (cairo-set-source cr background)
      (cairo_fill_preserve cr))
    (unless (not border)
      (cairo-set-stroke cr border)
      (cairo_stroke_preserve cr))))

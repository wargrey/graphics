#lang racket

(provide (all-defined-out))

(require racket/draw/unsafe/cairo)
(require bitmap/digitama/convert)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://www.cairographics.org/samples/gradient/

(define (cairo-gradient xc yc radius light shadow density)
  (define-values (width height) (values (* (max xc radius) 2.0) (* (max yc radius) 2.0)))
  (define-values (bmp cr) (create-argb-bitmap width height density #true))

  (define background (cairo_pattern_create_linear 0.0 0.0 0.0 height))
  (apply cairo_pattern_add_color_stop_rgba background 0.0 light)
  (apply cairo_pattern_add_color_stop_rgba background 1.0 shadow)
  (cairo_rectangle cr 0.0 0.0 width height)
  (cairo_set_source cr background)
  (cairo_fill cr)
  (cairo_pattern_destroy background)

  (define ball (cairo_pattern_create_radial 115.2 102.4 25.6 102.4 102.4 128.0))
  (apply cairo_pattern_add_color_stop_rgba ball 0.0 light)
  (apply cairo_pattern_add_color_stop_rgba ball 1.0 shadow)
  (cairo_set_source cr ball)
  (cairo_arc cr xc yc radius 0.0 (degrees->radians 360.0))
  (cairo_fill cr)
  (cairo_pattern_destroy ball)
  
  bmp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (cairo-gradient 128.0 128.0 76.8 '(1.0 1.0 1.0 1.0) '(0.0 0.0 0.0 1.0) 2.0))

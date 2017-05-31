#lang racket

(require "../../digitama/unsafe/pangocairo.rkt")

;;; https://www.cairographics.org/samples/curve_to/

(define density 2.0)

(define (cairo-curve x y x1 y1 x2 y2 x3 y3)
  (define-values (curve-width line-width) (values 10.0 6.0))
  (define width (+ (max x x1 x2 x3) (max curve-width line-width)))
  (define height (+ (max y y1 y2 y3) (max curve-width line-width)))
  (define-values (bmp cr _w _h) (make-cairo-image width height density))

  (cairo_move_to cr x y)
  (cairo_curve_to cr x1 y1 x2 y2 x3 y3)
  (cairo_set_line_width cr curve-width)
  (cairo_stroke cr)
  
  (cairo_move_to cr x y)
  (cairo_line_to cr x1 y1)
  (cairo_move_to cr x2 y2)
  (cairo_line_to cr x3 y3)
  (cairo_set_source_rgba cr 1.0 0.2 0.2 0.6)
  (cairo_set_line_width cr line-width)
  (cairo_stroke cr)
  
  bmp)

(define cairo-spline
  (case-lambda
    [(x1 y1 x2 y2) (cairo-curve x1 y1 x1 y1 x2 y2 x2 y2)]
    [(x1 y1 x2 y2 x3 y3) (cairo-curve x1 y1 x2 y2 x2 y2 x3 y3)]
    [(x1 y1 x2 y2 x3 y3 x4 y4) (cairo-curve x1 y1 x2 y2 x3 y3 x4 y4)]))

(cairo-spline 25.0 128.0 230.4 128.0)
(cairo-spline 25.0 128.0 102.4 230.4 153.6 25.6)
(cairo-spline 102.4 230.4 153.6 25.6 230.4 128.0)
(cairo-spline 25.0 128.0 102.4 230.4 153.6 25.6 230.4 128.0)

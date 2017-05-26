#lang racket

(require "../../digitama/unsafe/pangocairo.rkt")

;;; https://www.cairographics.org/samples/curve_rectangle/

(define density 2.0)

(define (cairo-curve-rectangle x0 y0 rect-width rect-height radius line-width)
  (define-values (x1 y1) (values (+ x0 rect-width) (+ y0 rect-height)))
  (define-values (width/2 height/2) (values (/ rect-width 2) (/ rect-height 2)))
  (define-values (bmp cr _w _h) (make-cairo-image (+ x1 line-width) (+ y1 line-width) density))

  (if (< width/2 radius)
      (if (< height/2 radius)
          (void (cairo_move_to cr x0 (/ (+ y0 y1) 2))
                (cairo_curve_to cr x0 y0 x0 y0 (/ (+ x0 x1) 2) y0)
                (cairo_curve_to cr x1 y0 x1 y0 x1 (/ (+ y0 y1) 2))
                (cairo_curve_to cr x1 y1 x1 y1 (/ (+ x1 x0) 2) y1)
                (cairo_curve_to cr x0 y1 x0 y1 x0 (/ (+ y0 y1) 2)))
          (void (cairo_move_to cr x0 (+ y0 radius))
                (cairo_curve_to cr x0 y0 x0 y0 (/ (+ x0 x1) 2) y0)
                (cairo_curve_to cr x1 y0 x1 y0 x1 (+ y0 radius))
                (cairo_line_to cr x1 (- y1 radius))
                (cairo_curve_to cr x1 y1 x1 y1 (/ (+ x1 x0) 2) y1)
                (cairo_curve_to cr x0 y1 x0 y1 x0 (- y1 radius))))
      (if (< height/2 radius)
          (void (cairo_move_to cr x0 (/ (+ y0 y1) 2))
                (cairo_curve_to cr x0 y0 x0 y0 (+ x0 radius) y0)
                (cairo_line_to cr (- x1 radius) y0)
                (cairo_curve_to cr x1 y0 x1 y0 x1 (/ (+ y0 y1) 2))
                (cairo_curve_to cr x1 y1 x1 y1 (- x1 radius) y1)
                (cairo_line_to cr (+ x0 radius) y1)
                (cairo_curve_to cr x0 y1 x0 y1 x0 (/ (+ y0 y1) 2)))
          (void (cairo_move_to cr x0 (+ y0 radius))
                (cairo_curve_to cr x0 y0 x0 y0 (+ x0 radius) y0)
                (cairo_line_to cr (- x1 radius) y0)
                (cairo_curve_to cr x1 y0 x1 y0 x1 (+ y0 radius))
                (cairo_line_to cr x1 (- y1 radius))
                (cairo_curve_to cr x1 y1 x1 y1 (- x1 radius) y1)
                (cairo_line_to cr (+ x0 radius) y1)
                (cairo_curve_to cr x0 y1 x0 y1 x0 (- y1 radius)))))
  (cairo_close_path cr)
  
  (cairo_set_source_rgb cr 0.5 0.5 1.0)
  (cairo_fill_preserve cr)
  (cairo_set_source_rgba cr 0.5 0.0 0.0 0.5)
  (cairo_set_line_width cr line-width)
  (cairo_stroke cr)
  
  bmp)

(define (cairo-rounded-rectangle x0 y0 width height aspect line-width)
  (define-values (x1 y1) (values (+ x0 width) (+ y0 height)))
  (define-values (bmp cr _w _h) (make-cairo-image (+ x1 line-width) (+ y1 line-width) density))
  (define radius (/ height 10.0 aspect))

  ;(cairo_new_sub_path cr) ; not neccessary
  (cairo_arc cr (- x1 radius) (+ y0 radius) radius (degrees->radians -90.0) 0.0)
  (cairo_arc cr (- x1 radius) (- y1 radius) radius 0.0 (degrees->radians 90.0))
  (cairo_arc cr (+ x0 radius) (- y1 radius) radius (degrees->radians 90.0) (degrees->radians 180.0))
  (cairo_arc cr (+ x0 radius) (+ y0 radius) radius (degrees->radians 180.0) (degrees->radians 270.0))
  (cairo_close_path cr)
  
  (cairo_set_source_rgb cr 0.5 0.5 1.0)
  (cairo_fill_preserve cr)
  (cairo_set_source_rgba cr 0.5 0.0 0.0 0.5)
  (cairo_set_line_width cr line-width)
  (cairo_stroke cr)
  
  bmp)

(cairo-curve-rectangle 10.24 10.24 81.92 81.92 40.96 1.00)
(cairo-curve-rectangle 10.24 10.24 81.92 81.92 40.96 2.00)
(cairo-curve-rectangle 10.24 10.24 81.92 81.92 40.96 8.00)

(cairo-rounded-rectangle 10.24 10.24 81.92 81.92 1.0 1.00)
(cairo-rounded-rectangle 10.24 10.24 81.92 81.92 1.0 2.00)
(cairo-rounded-rectangle 10.24 10.24 81.92 81.92 1.0 8.00)

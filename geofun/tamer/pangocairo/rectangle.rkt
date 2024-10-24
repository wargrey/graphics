#lang racket

(require racket/draw/unsafe/cairo)
(require bitmap/digitama/convert)

;;; https://www.cairographics.org/samples/curve_rectangle/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define density 2.0)

(define (cairo-curve-rectangle rect-width rect-height radius line-width)
  (define offset (* line-width 0.5))
  (define-values (x0 y0) (values offset offset))
  (define-values (x1 y1) (values (- rect-width offset) (- rect-height offset)))
  (define-values (width/2 height/2) (values (* rect-width 0.5) (* rect-height 0.5)))
  (define-values (bmp cr) (create-argb-bitmap rect-width rect-height density #true))

  (if (< width/2 radius)
      (if (< height/2 radius)
          (void (cairo_move_to cr x0 (* (+ y0 y1) 0.5))
                (cairo_curve_to cr x0 y0 x0 y0 (* (+ x0 x1) 0.5) y0)
                (cairo_curve_to cr x1 y0 x1 y0 x1 (* (+ y0 y1) 0.5))
                (cairo_curve_to cr x1 y1 x1 y1 (* (+ x1 x0) 0.5) y1)
                (cairo_curve_to cr x0 y1 x0 y1 x0 (* (+ y0 y1) 0.5)))
          (void (cairo_move_to cr x0 (+ y0 radius))
                (cairo_curve_to cr x0 y0 x0 y0 (* (+ x0 x1) 0.5) y0)
                (cairo_curve_to cr x1 y0 x1 y0 x1 (+ y0 radius))
                (cairo_line_to cr x1 (- y1 radius))
                (cairo_curve_to cr x1 y1 x1 y1 (* (+ x1 x0) 0.5) y1)
                (cairo_curve_to cr x0 y1 x0 y1 x0 (- y1 radius))))
      (if (< height/2 radius)
          (void (cairo_move_to cr x0 (* (+ y0 y1) 0.5))
                (cairo_curve_to cr x0 y0 x0 y0 (+ x0 radius) y0)
                (cairo_line_to cr (- x1 radius) y0)
                (cairo_curve_to cr x1 y0 x1 y0 x1 (* (+ y0 y1) 0.5))
                (cairo_curve_to cr x1 y1 x1 y1 (- x1 radius) y1)
                (cairo_line_to cr (+ x0 radius) y1)
                (cairo_curve_to cr x0 y1 x0 y1 x0 (* (+ y0 y1) 0.5)))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (cairo-curve-rectangle 81.92 81.92 40.96 1.00)
  (cairo-curve-rectangle 81.92 81.92 40.96 2.00)
  (cairo-curve-rectangle 81.92 81.92 40.96 8.00))

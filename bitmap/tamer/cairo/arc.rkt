#lang racket

(require "../../digitama/unsafe/pangocairo.rkt")

;;; https://www.cairographics.org/samples/arc/
;;; https://www.cairographics.org/samples/arc_negative/

(define density 2.0)

(define (cairo-arc xc yc radius start end negative-arc? circle-width line-width)
  (define-values (width height) (values (* (max xc radius) 2.0) (* (max yc radius) 2.0)))
  (define-values (angle1 angle2) (values (degrees->radians start) (degrees->radians end)))
  (define-values (bmp cr) (make-cairo-image width height density))
  (define draw-arc (if negative-arc? cairo_arc_negative cairo_arc))
  
  (cairo_translate cr xc yc)
  (cairo_set_line_width cr circle-width)
  (draw-arc cr 0 0 radius angle1 angle2)
  (cairo_stroke cr)
  
  (cairo_set_source_rgba cr 1.0 0.2 0.2 0.6)
  (cairo_arc cr 0 0 10.0 0.0 (degrees->radians 360.0))
  (cairo_fill cr)
  
  (cairo_set_line_width cr line-width)
  (draw-arc cr 0 0 radius angle1 angle1)
  (cairo_line_to cr 0 0)
  (draw-arc cr 0 0 radius angle2 angle2)
  (cairo_line_to cr 0 0)
  (cairo_stroke cr)
  
  bmp)

(cairo-arc 128.0 128.0 100.0 45.0 180.0 #false 10.0 6.0)
(cairo-arc 128.0 128.0 100.0 45.0 180.0 #true  10.0 6.0)

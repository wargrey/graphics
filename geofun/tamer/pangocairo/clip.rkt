#lang racket

(require "../../digitama/unsafe/pangocairo.rkt")
(require bitmap/digitama/convert)

;;; https://www.cairographics.org/samples/clip/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define density 2.0)

(define (cairo-clip xc yc radius)
  (define-values (width height) (values (* (max xc radius) 2.0) (* (max yc radius) 2.0)))
  (define-values (bmp cr) (create-argb-bitmap width height density #true))

  ;(cairo_tanslate cr xc yc) ; TODO: Does this operation affects angles?
  (cairo_arc cr xc yc radius 0 (degrees->radians 360.0))
  (cairo_clip cr)

  (define-values (x1 y1 x2 y2) (cairo_clip_extents cr))

  (cairo_new_path cr)
  (cairo_rectangle cr x1 y1 x2 y2)
  (cairo_fill cr)
  
  (cairo_set_source_rgb cr 0 1 0)
  (cairo_move_to cr x1 y1)
  (cairo_line_to cr x2 y2)
  (cairo_move_to cr x2 y1)
  (cairo_line_to cr x1 y2)
  (cairo_set_line_width cr 10.0)
  (cairo_stroke cr)
  
  bmp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (cairo-clip 128.0 128.0 76.0))

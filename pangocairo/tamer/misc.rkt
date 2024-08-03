#lang racket

(require "../digitama/unsafe/pangocairo.rkt")
(require bitmap/digitama/unsafe/surface)

;;; https://www.cairographics.org/samples/dash/
;;; https://www.cairographics.org/samples/fill_and_stroke2/
;;; https://www.cairographics.org/samples/fill_style/
;;; https://www.cairographics.org/samples/set_line_cap/
;;; https://www.cairographics.org/samples/set_line_join/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define density 2.0)

(define (cairo-path width height offset line-width dashes)
  (define-values (bmp cr) (create-argb-bitmap width height density #true))

  (cairo_move_to cr 128.0 25.6)
  (cairo_line_to cr 230.4 230.4)
  (cairo_rel_line_to cr -102.4 0.0)
  (cairo_curve_to cr 51.2 230.4 51.2 128.0 128.0 128.0)
  (cairo_close_path cr)
  
  (cairo_move_to cr 64.0 25.6)
  (cairo_rel_line_to cr 51.2 51.2)
  (cairo_rel_line_to cr -51.2 51.2)
  (cairo_rel_line_to cr -51.2 -51.2)
  (cairo_close_path cr)

  (cairo_set_dash cr dashes offset)
  (cairo_set_line_width cr line-width)
  (cairo_set_source_rgb cr 0.0 0.0 1.0)
  (cairo_fill_preserve cr)
  (cairo_set_source_rgb cr 0.0 0.0 0.0)
  (cairo_stroke cr)
  
  bmp)

(define (cairo-fill-style width height style line-width rgb)
  (define-values (bmp cr) (create-argb-bitmap width height density #true))

  (cairo_set_line_width cr line-width)
  
  (cairo_rectangle cr 12.0 12.0 232.0 70.0)
  (cairo_new_sub_path cr)
  (cairo_arc cr 64.0 64.0 40.0 0.0 (degrees->radians 360))
  (cairo_new_sub_path cr)
  (cairo_arc_negative cr 192.0 64.0 40.0 0.0 (degrees->radians -360))
  
  (cairo_set_fill_rule cr style)
  (cairo_set_source_rgb cr (vector-ref rgb 0) (vector-ref rgb 1) (vector-ref rgb 2))
  (cairo_fill_preserve cr)
  (cairo_set_source_rgb cr 0.0 0.0 0.0)
  (cairo_stroke cr)
  
  bmp)

(define (cairo-line-width width height line-width)
  (define-values (bmp cr) (create-argb-bitmap width height density #true))

  (cairo_set_line_width cr line-width)
  (cairo_rectangle cr 0.0 0.0 width height)
  (cairo_set_source_rgba cr 0.0 1.0 0.0 1.0)
  (cairo_stroke cr)

  (cairo_move_to cr (/ line-width 2.0) (/ line-width 2.0))
  (cairo_rel_line_to cr width 0.0)
  (cairo_set_source_rgba cr 0.0 0.0 0.0 1.0)
  (cairo_stroke cr)

  (cairo_set_line_width cr 1.0)
  (cairo_move_to cr 1.0 1.0)
  (cairo_rel_line_to cr 0.0 height)
  (cairo_set_source_rgba cr 0.0 0.0 0.0 1.0)
  (cairo_stroke cr)
  
  bmp)

(define (cairo-line-cap width height line-cap)
  (define-values (bmp cr) (create-argb-bitmap width height density #true))
  (define spacing (/ height 2.0))

  (cairo_set_line_width cr height)
  (cairo_set_line_cap cr line-cap)
  (cairo_move_to cr spacing spacing)
  (cairo_line_to cr (- width spacing) spacing)
  (cairo_stroke cr)
  
  (cairo_set_source_rgb cr 1.0 0.2 0.2)
  (cairo_set_line_width cr (/ height 10.0))
  (cairo_move_to cr spacing spacing)
  (cairo_line_to cr (- width spacing) spacing)
  (cairo_stroke cr)
  
  bmp)

(define (cairo-line-join width height line-join miter-limit)
  (define-values (bmp cr) (create-argb-bitmap width height density #true))
  (define line-width (/ height 3.0))
  (define delta (/ height (sqrt 2.0)))

  (cairo_translate cr (- (/ width 2.0) delta) (+ delta (* line-width (sqrt 2.0) 0.5)))
  (cairo_set_line_width cr line-width)
  (cairo_move_to cr 0.0 0.0)
  (cairo_rel_line_to cr delta (- delta))
  (cairo_rel_line_to cr delta delta)
  (cairo_set_line_join cr line-join)
  (cairo_set_miter_limit cr miter-limit)
  (cairo_stroke cr)
  
  bmp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (cairo-path 256.0 256.0 -50.0 8.0 #(32.0 8.0 8.0 8.0))
  
  (cairo-fill-style 256.0 128.0 CAIRO_FILL_RULE_EVEN_ODD 4.0 #(0.0 0.7 0.0))
  (cairo-fill-style 256.0 128.0 CAIRO_FILL_RULE_WINDING  4.0 #(0.0 0.0 0.9))
  
  (cairo-line-width 256.0 16.0 2.0)
  (cairo-line-width 256.0 16.0 1.0)
  (cairo-line-width 256.0 16.0 0.4)
  
  (cairo-line-cap 256.0 32.0 CAIRO_LINE_CAP_BUTT)    ; default
  (cairo-line-cap 256.0 32.0 CAIRO_LINE_CAP_ROUND)
  (cairo-line-cap 256.0 32.0 CAIRO_LINE_CAP_SQUARE)
  
  (cairo-line-join 256.0 64.0 CAIRO_LINE_JOIN_MITER 10.00) ; default
  (cairo-line-join 256.0 64.0 CAIRO_LINE_JOIN_MITER 2.000)
  (cairo-line-join 256.0 64.0 CAIRO_LINE_JOIN_MITER 1.414)
  (cairo-line-join 256.0 64.0 CAIRO_LINE_JOIN_MITER -10.0)
  (cairo-line-join 256.0 64.0 CAIRO_LINE_JOIN_BEVEL 10.00)
  (cairo-line-join 256.0 64.0 CAIRO_LINE_JOIN_ROUND 10.00))
  
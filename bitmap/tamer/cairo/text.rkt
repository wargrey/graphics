#lang racket

(require "../../digitama/unsafe/pangocairo.rkt")

(require racket/draw/private/utils)

(define-enum 0 CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_SLANT_ITALIC CAIRO_FONT_SLANT_OBLIQUE)
(define-enum 0 CAIRO_FONT_WEIGHT_NORMAL CAIRO_FONT_WEIGHT_BOLD)

(define-cstruct _cairo_text_extents_t
  ([x_bearing _double*]
   [y_bearing _double*]
   [width _double*]
   [height _double*]
   [x_advance _double*]
   [y_advance _double*])
  #:malloc-mode 'atomic-interior)

(define-cairo cairo_select_font_face (_cfun _cairo_t _string _int _int -> _void))
(define-cairo cairo_set_font_size (_cfun _cairo_t _double* -> _void))
(define-cairo cairo_show_text (_cfun _cairo_t _string -> _void))
(define-cairo cairo_text_path (_cfun _cairo_t _string -> _void))

(define-cairo cairo_text_extents (_cfun _cairo_t _string _cairo_text_extents_t-pointer -> _void))

;;; https://www.cairographics.org/samples/text/
;;; https://www.cairographics.org/samples/text_align_center/
;;; https://www.cairographics.org/samples/text_extents/

(define density 2.0)
(define-values (_b tcr _w _h) (make-cairo-image 1.0 1.0 density))

(define (cairo-text-size! face size style weight utf8 &extents)
  (cairo_select_font_face tcr face style weight)
  (cairo_set_font_size tcr size)
  (cairo_text_extents tcr utf8 &extents)
  
  (values (+ (cairo_text_extents_t-width &extents) ; === x_advance
             (* 2 (cairo_text_extents_t-x_bearing &extents)))
          (cairo_text_extents_t-height &extents)
          (- (cairo_text_extents_t-y_bearing &extents))))

(define (cairo-text face size style weight word word-x yw path-word path-x yp)
  (define &extents (make-cairo_text_extents_t 0.0 0.0 0.0 0.0 0.0 0.0))
  (define-values (border-width dot-radius) (values 2.56 5.12))
  
  (define-values (word-width word-height word-baseline) (cairo-text-size! face size style weight word &extents))
  (define-values (path-width path-height path-baseline) (cairo-text-size! face size style weight path-word &extents))
  (define-values (word-y path-y) (values (+ yw word-baseline) (+ yp path-baseline)))
  (define width (+ (max (+ word-x word-width) (+ path-x path-width)) border-width))
  (define height (+ (max (+ yw word-height) (+ yp path-height)) border-width))
  (define-values (bmp cr _w _h) (make-cairo-image width height density))

  (cairo_select_font_face cr face style weight)
  (cairo_set_font_size cr size)

  (cairo_move_to cr word-x (+ yw word-baseline))
  (cairo_show_text cr word)

  (cairo_move_to cr path-x path-y)
  (cairo_text_path cr path-word)
  (cairo_set_source_rgb cr 0.5 0.5 1)
  (cairo_fill_preserve cr)
  (cairo_set_source_rgb cr 0.0 0.0 0.0)
  (cairo_set_line_width cr border-width)
  (cairo_stroke cr)
  
  (cairo_set_source_rgba cr 1.0 0.2 0.2 0.6)
  (cairo_arc cr word-x (+ yw word-height) dot-radius 0.0 (degrees->radians 360.0))
  (cairo_close_path cr)
  (cairo_arc cr path-x path-y dot-radius 0.0 (degrees->radians 360.0))
  (cairo_fill cr)
  
  bmp)

(define (cairo-text-align flwidth flheight face size style weight word)
  (define &extents (make-cairo_text_extents_t 0.0 0.0 0.0 0.0 0.0 0.0))
  (define-values (dot-radius line-width) (values 10.0 6.0))
  
  (define-values (word-width word-height word-baseline) (cairo-text-size! face size style weight word &extents))
  (define-values (bmp cr width height) (make-cairo-image (max flwidth word-width) (max flheight word-height) density))
  (define-values (x y) (values (/ (- width word-width) 2.0) (+ (/ (- height word-height) 2.0) word-height)))

  (cairo_select_font_face cr face style weight)
  (cairo_set_font_size cr size)

  (cairo_move_to cr x y)
  (cairo_show_text cr word)
  
  (cairo_set_source_rgba cr 1.0 0.2 0.2 0.6)
  (cairo_arc cr x y dot-radius 0.0 (degrees->radians 360.0))
  (cairo_fill cr)

  (cairo_set_line_width cr line-width)
  (cairo_move_to cr (/ width 2.0) 0.0)
  (cairo_rel_line_to cr 0.0 height)
  (cairo_move_to cr 0.0 (/ height 2.0))
  (cairo_rel_line_to cr width 0.0)
  (cairo_stroke cr)
  
  bmp)

(define (cairo-text-extents face size style weight word x0 y0)
  (define &extents (make-cairo_text_extents_t 0.0 0.0 0.0 0.0 0.0 0.0))
  (define-values (dot-radius line-width) (values 10.0 6.0))
  (define-values (x y) (values (+ x0 line-width) (+ y0 line-width)))
  (define-values (word-width word-height baseline) (cairo-text-size! face size style weight word &extents))
  (define-values (bmp cr _w _h) (make-cairo-image (+ x word-width line-width) (+ y word-height) density))

  (cairo_select_font_face cr face style weight)
  (cairo_set_font_size cr size)

  (cairo_move_to cr x (+ y baseline))
  (cairo_show_text cr word)
  
  (cairo_set_source_rgba cr 1.0 0.2 0.2 0.6)
  (cairo_arc cr x (+ y word-height) dot-radius 0.0 (degrees->radians 360.0))
  (cairo_fill cr)

  (cairo_set_line_width cr line-width)
  (cairo_move_to cr x (+ y word-height))
  (cairo_rel_line_to cr 0.0 (- word-height))
  (cairo_rel_line_to cr word-width 0.0)
  (cairo_rel_line_to cr (cairo_text_extents_t-x_bearing &extents) baseline)
  (cairo_stroke cr)
  
  bmp)

(cairo-text "Courier" 64.0 CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_BOLD "Hello" 0.0 0.0 "Cairo" 42.0 20.0)
(cairo-text-align 256.0 128.0 "monospace" 52.0 CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_NORMAL "Center")
(cairo-text-extents "Sans" 80.0 CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_NORMAL "Sphinx" 0.0 0.0)

#lang racket

(provide (all-defined-out))

(require racket/math)
(require racket/flonum)

(require "../digitama/unsafe/ffi.rkt")
(require (submod "../digitama/unsafe/font.rkt" unsafe))
(require (submod "../digitama/unsafe/image.rkt" unsafe))

(define density 2.0)

(define (cairo-text-circle radius)
  (define-values (bmp cr width height) (make-cairo-image (* 2.0 radius) (* 2.0 radius) density))

  (define (draw-text-circle cr words font-face font-size font-style font-weight font-stretch)
    ; Create a PangoLayout, set the font and text
    (define layout (pango_cairo_create_layout cr))
    (define desc (bitmap_create_font_desc font-face font-size font-style font-weight font-stretch))
    (pango_layout_set_font_description layout desc)
    (pango_font_description_free desc)

    (define n (length words))
    (for ([i (in-range n)])
      (define angle (/ (* 360.0 i) n))
      (cairo_save cr)

      ; Gradient from red at angle == 60 to blue at angle == 240
      (define red (/ (+ 1 (cos (degrees->radians (- angle 60)))) 2))
      (pango_layout_set_text layout (list-ref words i))
      (cairo_set_source_rgb cr red 0 (- 1.0 red))
      (cairo_rotate cr (degrees->radians angle))

      ; Inform Pango to re-layout the text with the new transformation
      (pango_cairo_update_layout cr layout)

      (define-values (width height) (pango_layout_get_size layout))
      (cairo_move_to cr (/ (~metric width) -2) (- radius))
      (pango_cairo_show_layout cr layout)

      (cairo_restore cr)))

  ; draw background squares
  (for* ([x (in-range (/ width 10.0))]
         [y (in-range (/ height 10.0))])
    (cairo_rectangle cr (* x 10.0) (* y 10) 5 5))
  
  (define brush (cairo_pattern_create_radial radius radius (/ radius 3.0) radius radius radius))
  (cairo_pattern_set_extend brush CAIRO_EXTEND_PAD)
  (cairo_pattern_add_color_stop_rgba brush 0.0 (random) (random) (random) 1.0)
  (cairo_pattern_add_color_stop_rgba brush 0.9 1.0 1.0 1.0 1.0)
  (cairo_set_source cr brush)
  (cairo_fill cr)

  ; Center coordinates on the middle of the region we are drawing
  (cairo_translate cr radius radius)  
  
  (define words (string-split "Using Pango with Cairo to Draw Regular Polygon"))
  (draw-text-circle cr words "Helvetica Neue" (* radius 0.16) 'normal 'normal 'expanded)
  (cairo_destroy cr)
  (cairo_pattern_destroy brush)
  bmp)

(define (cairo-paragraph)
  (define pattern (cairo_pattern_create_linear 0.0 0.0 256.0 128.0))
  (cairo_pattern_set_extend pattern CAIRO_EXTEND_PAD)
  (cairo_pattern_add_color_stop_rgba pattern 0.0 1.0 0.0 0.0 1.0)
  (cairo_pattern_add_color_stop_rgba pattern 0.5 0.0 1.0 0.0 1.0)
  (cairo_pattern_add_color_stop_rgba pattern 1.0 0.0 0.0 1.0 1.0)
  (bitmap_paragraph (string-append "Pango Layout Test:\n"
                                   "Here is some text that should wrap suitably to demonstrate PangoLayout's features.\n"
                                   "This paragraph should be ellipsized or truncated.")
                    256.0 128.0 32.0 4.0 PANGO_WRAP_WORD_CHAR PANGO_ELLIPSIZE_END
                    (bitmap_create_font_desc "Trebuchet MS" 16.0 'normal 'normal 'normal)
                    pattern (flvector (random) (random) (random) 0.08) density))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (benchmark make-image . args)
  (printf "~a " (object-name make-image))
  (collect-garbage)
  (time (apply make-image args)))

(benchmark cairo-text-circle 150)
(benchmark cairo-paragraph)

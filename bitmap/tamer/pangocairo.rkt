#lang racket

(provide (all-defined-out))

(require racket/math)
(require racket/flonum)

(require "../digitama/unsafe/pangocairo.rkt")
(require (submod "../digitama/unsafe/font.rkt" unsafe))
(require (submod "../digitama/unsafe/image.rkt" unsafe))

(define (cairo-text-circle words radius font-face font-weight font-attrs)
  (define-values (bmp cr width height) (make-cairo-image (* 2.0 radius) (* 2.0 radius) density))
  (define (draw-text-circle cr context words)
    (define layout (pango_layout_new context))
    (when font-attrs (pango_layout_set_attributes layout font-attrs))

    (define n (length words))
    (for ([i (in-range n)])
      (define angle (/ (* 360.0 i) n))
      (cairo_save cr)

      ; Gradient from red at angle == 60 to blue at angle == 240
      (define red (/ (+ 1 (cos (degrees->radians (- angle 60)))) 2))
      (pango_layout_set_text layout (list-ref words i))
      (cairo_set_source_rgb cr red 0 (- 1.0 red))
      (cairo_rotate cr (degrees->radians angle))

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

  (define desc (bitmap_create_font_desc font-face (* radius 0.16) 'normal font-weight 'normal))

  ; Center coordinates on the middle of the region we are drawing
  (cairo_translate cr radius radius)
  (pango_context_set_font_description context desc)
  ; (pango_cairo_update_context cr context) ; this is not neccessary?
  
  (draw-text-circle cr context (string-split words))
  
  (pango_font_description_free desc)
  (cairo_destroy cr)
  (cairo_pattern_destroy brush)
  bmp)

(define (cairo-paragraph)
  (define-values (width height indent spacing) (values 256.0 128.0 32.0 4.0))
  (define pattern (cairo_pattern_create_linear 0.0 0.0 width height))
  (cairo_pattern_set_extend pattern CAIRO_EXTEND_PAD)
  (cairo_pattern_add_color_stop_rgba pattern 0.0 1.0 0.0 0.0 1.0)
  (cairo_pattern_add_color_stop_rgba pattern 0.5 0.0 1.0 0.0 1.0)
  (cairo_pattern_add_color_stop_rgba pattern 1.0 0.0 0.0 1.0 1.0)
  (bitmap_paragraph (string-append (format "Layout Box(~a, ~a):\n" width height)
                                   "Here is some text that should wrap suitably to demonstrate PangoLayout's features.\n"
                                   "This paragraph should be ellipsized or truncated.")
                    width height indent spacing PANGO_WRAP_WORD_CHAR PANGO_ELLIPSIZE_END
                    (bitmap_create_font_desc "Trebuchet MS" 16.0 'normal 'normal 'normal)
                    '(undercurl) #false
                    pattern (flvector (random) (random) (random) 0.08) density))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (benchmark make-image . args)
  (define smart-fmt (if (terminal-port? (current-output-port)) "\033[38;5;32m~a\033[0m " "~a "))
  (printf smart-fmt (object-name make-image))
  (collect-garbage)
  (time (apply make-image args)))

(define density 2.0)
(define pango-font-map (benchmark pango_cairo_font_map_get_default))
(define context (pango_font_map_create_context pango-font-map))
(define font-options (benchmark cairo_font_options_create))

(define double-attrs (pango_attr_list_new))
(pango_attr_list_insert double-attrs (pango_attr_underline_new PANGO_UNDERLINE_DOUBLE))

(define delete-attrs (pango_attr_list_new))
(pango_attr_list_insert delete-attrs (pango_attr_strikethrough_new #true))

(benchmark cairo-text-circle "Using Pango with Cairo to Draw Regular Polygon" 150 "Courier" 'bold double-attrs)
(benchmark cairo-text-circle "Test Global Cairo Context and Pango Layout" 100 "Helvetica Neue" 'thin delete-attrs)
(benchmark cairo-paragraph)

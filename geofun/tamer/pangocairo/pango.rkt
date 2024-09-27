#lang racket

(provide (all-defined-out))

(require geofun/font)
(require bitmap/constructor)
(require bitmap/digitama/convert)
;(require bitmap/digitama/stdio)

(require "../../digitama/base.rkt")
(require "../../digitama/unsafe/pangocairo.rkt")
(require (submod "../../digitama/unsafe/dc/text.rkt" unsafe))
(require (only-in (submod "../../digitama/unsafe/font.rkt" unsafe) geo_create_font_desc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (cairo_text_polygon words radius font-face font-weight font-attr-lines)
  (define-values (width height) (values (* 2.0 radius) (* 2.0 radius)))
  (define-values (bmp cr) (create-argb-bitmap width height density #true))
  
  (define (draw-text-circle cr layout words)
    (define n (length words))
    (for ([i (in-range n)])
      (define angle (/ (* 360.0 i) n))
      (cairo_save cr)

      ; Gradient from red at 60deg to blue at 240deg
      (define c (/ (+ 1 (cos (degrees->radians (- angle 60)))) 2))
      (pango_layout_set_text layout (list-ref words i))
      (cairo_set_source_rgb cr c 0 (- 1.0 c))
      (cairo_rotate cr (degrees->radians angle))

      (define-values (width height) (pango_layout_get_size layout))
      (cairo_move_to cr (* (~metric width) -0.5) (* radius -1.0))
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

  (define desc (geo_create_font_desc font-face (* radius 0.16) font-weight 0 4 0))
  (define layout (text_create_layout font-attr-lines))
  (pango_layout_set_font_description layout desc)

  ; Center coordinates on the middle of the region we are drawing
  (cairo_translate cr radius radius)
  (draw-text-circle cr layout (string-split words))
  
  (pango_font_description_free desc)
  (cairo_destroy cr)
  (cairo_pattern_destroy brush)
  bmp)

(define (cairo-text-polygon words radius font-face font-weight font-attrs)
  (define bmp (cairo_text_polygon words radius font-face font-weight font-attrs))
  (define temp.png (make-temporary-file))
  (displayln temp.png)
  (cairo_surface_write_to_png (bitmap-surface bmp) temp.png)
  #;(read-bitmap temp.png #:backing-scale density)
  bmp)

(define (cairo-paragraph)
  (define-values (width height indent spacing) (values 256.0 128.0 32.0 4.0))
  (define pattern (cairo_pattern_create_linear 0.0 0.0 width height))
  (cairo_pattern_set_extend pattern CAIRO_EXTEND_PAD)
  (cairo_pattern_add_color_stop_rgba pattern 0.0 1.0 0.0 0.0 1.0)
  (cairo_pattern_add_color_stop_rgba pattern 0.5 0.0 1.0 0.0 1.0)
  (cairo_pattern_add_color_stop_rgba pattern 1.0 0.0 0.0 1.0 1.0)
  (bitmap-paragraph (list (format "Layout Box(~a, ~a):" width height)
                          "Here is some text that should wrap suitably to demonstrate PangoLayout's features."
                          "This paragraph should be ellipsized or truncated.")
                    (desc-font #:family "Trebuchet MS" #:size 16.0 #:weight 'medium #:style 'normal #:stretch 'normal)
                    #:max-width width #:max-height height #:indent indent #:spacing spacing
                    #:wrap-mode 'word-char #:ellipsize-mode 'end #:lines '(undercurl)
                    #:color pattern #:background (rgba (random) (random) (random) 0.2) #:density density))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (benchmark make-image . args)
  (define smart-fmt (if (terminal-port? (current-output-port)) "\033[38;5;32m~a\033[0m " "~a "))
  (printf smart-fmt (object-name make-image))
  (collect-garbage)
  (time (apply make-image args)))

(define (current-memory-mb)
  (define mem (current-memory-use))
  (/ mem 1024.0 1024.0))

(define density 2.0)

(module+ main
  (benchmark cairo-text-polygon "Using Pango with Cairo to Draw Regular Polygon" 150 "Courier" 700 '(underdouble))
  (benchmark cairo-text-polygon "Test Global Cairo Context and Pango Layout" 100 "Helvetica Neue" 100 '(line-through))
  (benchmark cairo-paragraph))
  
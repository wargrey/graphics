#lang typed/racket/base

(provide (all-defined-out))

(require "../draw.rkt")
(require "font.rkt")
(require "source.rkt")
(require "require.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require "pangocairo.rkt")
  (require (submod "font.rkt" unsafe))
  
  (define (bitmap_paragraph words max-width max-height indent spacing wrap ellipsize font-desc lines fgsource bgsource density)
    (define layout (bitmap_create_layout the-cairo max-width max-height indent spacing wrap ellipsize))
    (pango_layout_set_font_description layout font-desc)
    (pango_layout_set_text layout words)

    (when (pair? lines)
      (define attrs (pango_attr_list_new))
      (when (memq 'line-through lines) (pango_attr_list_insert attrs (pango_attr_strikethrough_new #true)))
      (cond [(memq 'undercurl lines) (pango_attr_list_insert attrs (pango_attr_underline_new PANGO_UNDERLINE_ERROR))]
            [(memq 'underdouble lines) (pango_attr_list_insert attrs (pango_attr_underline_new PANGO_UNDERLINE_DOUBLE))]
            [(memq 'underline lines) (pango_attr_list_insert attrs (pango_attr_underline_new PANGO_UNDERLINE_SINGLE))])
      (pango_layout_set_attributes layout attrs)
      (pango_attr_list_unref attrs))

    (define-values (pango-width pango-height) (pango_layout_get_size layout))
    (define-values (flwidth flheight) (values (~metric pango-width) (unsafe-flmin (~metric pango-height) max-height)))
    (define-values (bmp cr draw-text?)
      (if (unsafe-fl<= flwidth max-width)
          (let-values ([(bmp cr w h) (make-cairo-image* flwidth flheight bgsource density)])
            (values bmp cr #true))
          (let-values ([(w h) (and (pango_layout_set_text layout " ") (pango_layout_get_size layout))])
            (define draw-text? (unsafe-fl>= max-width (~metric w)))
            (define smart-height (if draw-text? flheight (unsafe-flmin (~metric h) flheight)))
            (define-values (bmp cr _w _h) (make-cairo-image* max-width smart-height bgsource density))
            (values bmp cr draw-text?))))
    (when draw-text?
      (cairo-set-source cr fgsource)
      (cairo_move_to cr 0 0)
      (pango_cairo_show_layout cr layout))
    (cairo_destroy cr)
    bmp)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (bitmap_create_layout cr max-width max-height indent spacing wrap-mode ellipsize-mode)
    (define context (the-context))
    (define layout (pango_layout_new context))
    (pango_layout_set_width layout (if (flonum? max-width) (~size max-width) max-width))
    (pango_layout_set_height layout (if (flonum? max-height) (~size max-height) max-height))
    (pango_layout_set_indent layout (~size indent))   ; (~size nan.0) == (~size inf.0) == 0
    (pango_layout_set_spacing layout (~size spacing)) ; pango knows the minimum spacing
    (pango_layout_set_wrap layout wrap-mode)
    (pango_layout_set_ellipsize layout ellipsize-mode)
    layout)

  (define the-context
    (let ([&context (box #false)])
      (lambda []
        (or (unbox &context)
            (let ([fontmap (pango_cairo_font_map_get_default)])
              (define context (pango_font_map_create_context fontmap))
              (define options (cairo_font_options_create))
              (cairo_font_options_set_antialias options CAIRO_ANTIALIAS_DEFAULT)
              (pango_cairo_context_set_font_options context options)
              (set-box! &context context)
              (unbox &context)))))))

(unsafe/require/provide
 (submod "." unsafe)
 [bitmap_paragraph (-> String (U Integer Flonum) (U Integer Flonum) Flonum Flonum Integer Integer Font-Description (Listof Symbol)
                       Bitmap-Source Bitmap-Source Flonum Bitmap)])

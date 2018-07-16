#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "../base.rkt")
(require "convert.rkt")
(require "source.rkt")
(require "font.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require "pangocairo.rkt")
  (require (submod "font.rkt" unsafe))

  (define (bitmap_text text font-desc lines fgsource bgsource alsource dlsource clsource mlsource blsource density)
    (define-values (width height distance ascent descent) (font_get_text_extent font-desc text))
    (define-values (bmp cr) (make-text-image width height bgsource density))
    (define layout (bitmap_create_layout the-cairo lines))
    (pango_layout_set_font_description layout font-desc)
    (pango_layout_set_text layout text)
    (cairo-set-source cr fgsource)
    (cairo_move_to cr 0 0)
    (pango_cairo_show_layout cr layout)
    
    (when (or alsource clsource mlsource blsource dlsource)
      (define-values (ascent capline meanline baseline descent) (font_get_metrics_lines font-desc text))
      (cairo_set_line_width cr 1.0)
      (bitmap_decorate cr alsource ascent width)
      (bitmap_decorate cr clsource capline width)
      (bitmap_decorate cr mlsource meanline width)
      (bitmap_decorate cr blsource baseline width)
      (bitmap_decorate cr dlsource descent width))
    bmp)
  
  (define (bitmap_paragraph text font-desc lines max-width max-height indent spacing wrap ellipsize fgsource bgsource density)
    (define layout (bitmap_create_layout* the-cairo lines max-width max-height indent spacing wrap ellipsize))
    (pango_layout_set_font_description layout font-desc)
    (pango_layout_set_text layout text)

    (define-values (pango-width pango-height) (pango_layout_get_size layout))
    (define flwidth (~metric pango-width))
    (define flheight (if (flonum? max-height) (unsafe-flmin (~metric pango-height) max-height) (~metric pango-height)))
    (define-values (bmp cr draw-text?)
      (if (or (= max-width -1) (unsafe-fl<= flwidth max-width))
          (let-values ([(bmp cr) (make-text-image flwidth flheight bgsource density)])
            (values bmp cr #true))
          (let-values ([(char-width char-height) (and (pango_layout_set_text layout " ") (pango_layout_get_size layout))])
            (define draw-text? (unsafe-fl>= max-width (~metric char-width)))
            (define smart-flheight (if draw-text? flheight (unsafe-flmin (~metric char-height) flheight)))
            (define-values (bmp cr) (make-text-image max-width smart-flheight bgsource density))
            (values bmp cr draw-text?))))
    (when draw-text?
      (cairo-set-source cr fgsource)
      (cairo_move_to cr 0.0 0.0)
      (pango_cairo_show_layout cr layout))
    (cairo_destroy cr)
    bmp)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (make-text-image width height background density)
    (if (not background)
        (make-cairo-image width height density #true)
        (make-cairo-image* width height background density #true)))
  
  (define (bitmap_create_layout cr lines)
    (define context (the-context))
    (define layout (pango_layout_new context))
    (when (pair? lines)
      (define attrs (pango_attr_list_new))
      (when (memq 'line-through lines) (pango_attr_list_insert attrs (pango_attr_strikethrough_new #true)))
      (cond [(memq 'undercurl lines) (pango_attr_list_insert attrs (pango_attr_underline_new PANGO_UNDERLINE_ERROR))]
            [(memq 'underdouble lines) (pango_attr_list_insert attrs (pango_attr_underline_new PANGO_UNDERLINE_DOUBLE))]
            [(memq 'underline lines) (pango_attr_list_insert attrs (pango_attr_underline_new PANGO_UNDERLINE_SINGLE))])
      (pango_layout_set_attributes layout attrs)
      (pango_attr_list_unref attrs))
    layout)
  
  (define (bitmap_create_layout* cr lines width height indent spacing wrap-mode ellipsize-mode)
    (define layout (bitmap_create_layout cr lines))
    (pango_layout_set_width layout (if (flonum? width) (~size width) width #|-1|#))
    (pango_layout_set_height layout (if (flonum? height) (~size height) height))
    (pango_layout_set_indent layout (~size indent))   ; (~size nan.0) == (~size inf.0) == 0
    (pango_layout_set_spacing layout (~size spacing)) ; pango knows the minimum spacing
    (pango_layout_set_wrap layout wrap-mode)
    (pango_layout_set_ellipsize layout ellipsize-mode)
    layout)

  (define (bitmap_decorate cr color y width)
    (unless (not color)
      (cairo-set-source cr color)
      (cairo_move_to cr 0.0 y)
      (cairo_rel_line_to cr width 0.0)
      (cairo_stroke cr)))
  
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

(unsafe-require/typed/provide
 (submod "." unsafe)
 [bitmap_text
  (-> String Font-Description (Listof Symbol) Bitmap-Source (Option Bitmap-Source)
      (Option FlRGBA) (Option FlRGBA) (Option FlRGBA) (Option FlRGBA) (Option FlRGBA)
      Flonum Bitmap)]
 [bitmap_paragraph
  (-> String Font-Description (Listof Symbol) (U Flonum Nonpositive-Integer) (U Flonum Nonpositive-Integer)
      Flonum Flonum Integer Integer Bitmap-Source (Option Bitmap-Source)
      Flonum Bitmap)])

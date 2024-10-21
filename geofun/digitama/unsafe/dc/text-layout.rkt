#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "../../base.rkt")
(require "../source.rkt")
(require "../font.rkt")
(require "../visual/ctype.rkt")

(module unsafe racket/base
  (provide (all-defined-out))

  (require "../pangocairo.rkt")
  (require (submod "../paint.rkt" unsafe))
  (require (submod "../font.rkt" unsafe))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (dc_text cr x0 y0 width height text font-desc lines fgsource bgsource alsource clsource mlsource blsource dlsource)
    (define layout (text_create_layout lines))

    (cairo-render-background cr bgsource)
  
    (pango_layout_set_font_description layout font-desc)
    (pango_layout_set_text layout text)
    
    (cairo-set-source cr fgsource)
    (cairo_move_to cr x0 y0)
    (pango_cairo_show_layout cr layout)
    
    (when (or alsource clsource mlsource blsource dlsource)
      (define-values (ascent capline meanline baseline descent) (font_get_metrics_lines* font-desc text))
      (cairo_set_line_width cr 1.0)

      (text_decorate cr clsource x0 y0 capline width)
      (text_decorate cr alsource x0 y0 ascent width)
      (text_decorate cr mlsource x0 y0 meanline width)
      (text_decorate cr dlsource x0 y0 descent width)
      (text_decorate cr blsource x0 y0 baseline width)))

  (define (dc_art_text cr x0 y0 width height text font-desc lines stroke-source fill-source bgsource)
    (define layout (text_create_layout lines))

    (cairo-render-background cr bgsource)
  
    (pango_layout_set_font_description layout font-desc)
    (pango_layout_set_text layout text)

    (let* ([n (pango_layout_get_line_count layout)]
           [delta (unsafe-fl/ (unsafe-fl+ height (~pango-metric (pango_layout_get_spacing layout)))
                              (unsafe-fx->fl n))])
      (let draw_line ([idx 0]
                      [y (unsafe-fl+ y0 (~pango-metric (pango_layout_get_baseline layout)))])
        (when (unsafe-fx< idx n)
          (cairo_move_to cr x0 y)
          (pango_cairo_layout_line_path cr (pango_layout_get_line_readonly layout idx))
          (draw_line (unsafe-fx+ idx 1) (unsafe-fl+ y delta)))))
    
    (cairo-render cr stroke-source fill-source))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (dc_paragraph cr x0 y0 flwidth flheight text font-desc lines max-width max-height indent spacing wrap ellipsize fgsource bgsource)
    (define layout (text_create_layout* lines max-width max-height indent spacing wrap ellipsize))

    (pango_layout_set_font_description layout font-desc)
    (pango_layout_set_text layout text)

    (cairo-render-background cr bgsource)
    (cairo-set-source cr fgsource)
    (cairo_move_to cr x0 y0)
    (pango_cairo_show_layout cr layout))

  (define (dc_paragraph_size text font-desc lines max-width max-height indent spacing wrap ellipsize)
    (define layout (text_create_layout* lines max-width max-height indent spacing wrap ellipsize))

    (pango_layout_set_font_description layout font-desc)
    (pango_layout_set_text layout text)

    (define-values (pango-width pango-height) (pango_layout_get_size layout))
    (define flwidth (~pango-metric pango-width))
    (define flheight
      (if (flonum? max-height)
          (unsafe-flmin (~pango-metric pango-height) max-height)
          (~pango-metric pango-height)))

    (cond [(or (not max-width) (unsafe-fl<= flwidth max-width)) (values flwidth flheight)]
          [else (let-values ([(char-width char-height) (and (pango_layout_set_text layout " ") (pango_layout_get_size layout))])
                  (define draw-text? (unsafe-fl>= max-width (~pango-metric char-width)))
                  (define smart-flheight (if draw-text? flheight (unsafe-flmin (~pango-metric char-height) flheight)))
                  (values max-width smart-flheight))]))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (text_create_layout lines)
    (define context (the-context))
    (define layout (pango_layout_new context))
    (when (pair? lines)
      (define attrs (pango_attr_list_new))
      (when (memq 'line-through lines) (pango_attr_list_insert attrs (pango_attr_strikethrough_new #true)))
      ;(when (memq 'overline lines) (pango_attr_list_insert attrs (pango_attr_overline_new 1))) ; since Pango 1.46
      (cond [(memq 'undercurl lines) (pango_attr_list_insert attrs (pango_attr_underline_new PANGO_UNDERLINE_ERROR))]
            [(memq 'underdouble lines) (pango_attr_list_insert attrs (pango_attr_underline_new PANGO_UNDERLINE_DOUBLE))]
            [(memq 'underline lines) (pango_attr_list_insert attrs (pango_attr_underline_new PANGO_UNDERLINE_SINGLE))])
      (pango_layout_set_attributes layout attrs)
      (pango_attr_list_unref attrs))
    layout)
  
  (define (text_create_layout* lines width height indent spacing wrap-mode ellipsize-mode)
    (define layout (text_create_layout lines))
    (pango_layout_set_width layout (if (not width) -1 (~pango-size width)))
    (pango_layout_set_height layout (if (flonum? height) (~pango-size height) height))
    (pango_layout_set_indent layout (~pango-size indent))   ; (~pango-size nan.0) == (~pango-size inf.0) == 0
    (pango_layout_set_spacing layout (~pango-size spacing)) ; pango knows the minimum spacing
    (pango_layout_set_wrap layout wrap-mode)
    (pango_layout_set_ellipsize layout ellipsize-mode)
    layout)

  (define (text_decorate cr color x y yoff width)
    (unless (not color)
      (cairo-set-stroke cr color)
      (cairo_move_to cr x (unsafe-fl+ y yoff))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [dc_art_text
  (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
      String Font-Description (Listof Symbol)
      (Option Paint) (Option Fill-Source) (Option Fill-Source)
      Any)]
 
 [dc_text
  (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
      String Font-Description (Listof Symbol) Fill-Source (Option Fill-Source)
      (Option Paint) (Option Paint) (Option Paint) (Option Paint) (Option Paint)
      Any)]
 
 [dc_paragraph
  (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
      String Font-Description (Listof Symbol) (Option Flonum) (U Flonum Nonpositive-Integer)
      Flonum Flonum Integer Integer Fill-Source (Option Fill-Source)
      Any)]

 [dc_paragraph_size
  (-> String Font-Description (Listof Symbol) (Option Flonum) (U Flonum Nonpositive-Integer) Flonum Flonum Integer Integer
      (Values Nonnegative-Flonum Nonnegative-Flonum))])

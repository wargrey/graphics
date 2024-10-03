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
  (require "../paint.rkt")
  (require (submod "../font.rkt" unsafe))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (dc_text create-surface text font-desc lines fgsource bgsource alsource clsource mlsource blsource dlsource density)
    (define-values (width height) (font_get_text_extent font-desc text))
    (define-values (sfc cr) (create-surface width height density #true))
    (define layout (text_create_layout lines))

    (cairo-render-background cr bgsource)
  
    (pango_layout_set_font_description layout font-desc)
    (pango_layout_set_text layout text)
    
    (cairo-set-source cr fgsource)
    (cairo_move_to cr 0 0)
    (pango_cairo_show_layout cr layout)
    
    (when (or alsource clsource mlsource blsource dlsource)
      (define-values (ascent capline meanline baseline descent) (font_get_metrics_lines* font-desc text))
      (cairo_set_line_width cr 1.0)

      (text_decorate cr clsource capline width)
      (text_decorate cr alsource ascent width)
      (text_decorate cr mlsource meanline width)
      (text_decorate cr dlsource descent width)
      (text_decorate cr blsource baseline width))

    (cairo_destroy cr)

    sfc)
  
  (define (dc_paragraph create-surface text font-desc lines max-width max-height indent spacing wrap ellipsize fgsource bgsource density)
    (define layout (text_create_layout* lines max-width max-height indent spacing wrap ellipsize))
    (pango_layout_set_font_description layout font-desc)
    (pango_layout_set_text layout text)

    (define-values (pango-width pango-height) (pango_layout_get_size layout))
    (define flwidth (~metric pango-width))
    (define flheight (if (flonum? max-height) (unsafe-flmin (~metric pango-height) max-height) (~metric pango-height)))

    (define-values (sfc cr draw-text?)
      (if (or (not max-width) (unsafe-fl<= flwidth max-width))
          (let-values ([(sfc cr) (create-surface flwidth flheight density #true)])
            (cairo-render-background cr bgsource)
            (values sfc cr #true))
          (let-values ([(char-width char-height) (and (pango_layout_set_text layout " ") (pango_layout_get_size layout))])
            (define draw-text? (unsafe-fl>= max-width (~metric char-width)))
            (define smart-flheight (if draw-text? flheight (unsafe-flmin (~metric char-height) flheight)))
            (define-values (sfc cr) (create-surface max-width smart-flheight density #true))
            (cairo-render-background cr bgsource)
            (values sfc cr draw-text?))))

    (when draw-text?
      (cairo-set-source cr fgsource)
      (cairo_move_to cr 0.0 0.0)
      (pango_cairo_show_layout cr layout))
    
    (cairo_destroy cr)
    
    sfc)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (dc_art_text create-surface text font-desc lines stroke-source fill-source bgsource density)
    (define-values (width height) (font_get_text_extent font-desc text))
    (define-values (sfc cr) (create-surface width height density #true))
    (define layout (text_create_layout lines))

    (cairo-render-background cr bgsource)
  
    (pango_layout_set_font_description layout font-desc)
    (pango_layout_set_text layout text)

    (let* ([n (pango_layout_get_line_count layout)]
           [gap (~metric (pango_layout_get_spacing layout))]
           [baseline (~metric (pango_layout_get_baseline layout))]
           [delta (unsafe-fl/ (unsafe-fl+ height gap) (unsafe-fx->fl n))])
      (let draw_line ([idx 0]
                      [y baseline])
        (when (unsafe-fx< idx n)
          (cairo_move_to cr 0.0 y)
          (pango_cairo_layout_line_path cr (pango_layout_get_line_readonly layout idx))
          (draw_line (unsafe-fx+ idx 1) (unsafe-fl+ y delta)))))
    
    (cairo-render cr stroke-source fill-source)
    
    (cairo_destroy cr)

    sfc)
  
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
    (pango_layout_set_width layout (if (not width) -1 (~size width)))
    (pango_layout_set_height layout (if (flonum? height) (~size height) height))
    (pango_layout_set_indent layout (~size indent))   ; (~size nan.0) == (~size inf.0) == 0
    (pango_layout_set_spacing layout (~size spacing)) ; pango knows the minimum spacing
    (pango_layout_set_wrap layout wrap-mode)
    (pango_layout_set_ellipsize layout ellipsize-mode)
    layout)

  (define (text_decorate cr color y width)
    (unless (not color)
      (cairo-set-stroke cr color)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [dc_art_text
  (All (S) (-> (Cairo-Surface-Create S) String Font-Description (Listof Symbol)
               (Option Paint) (Option Fill-Source) (Option Fill-Source)
               Flonum S))]
 
 [dc_text
  (All (S) (-> (Cairo-Surface-Create S)
               String Font-Description (Listof Symbol) Fill-Source (Option Fill-Source)
               (Option Paint) (Option Paint) (Option Paint) (Option Paint) (Option Paint)
               Flonum S))]
 
 [dc_paragraph
  (All (S) (-> (Cairo-Surface-Create S)
               String Font-Description (Listof Symbol) (Option Flonum) (U Flonum Nonpositive-Integer)
               Flonum Flonum Integer Integer Fill-Source (Option Fill-Source)
               Flonum S))])

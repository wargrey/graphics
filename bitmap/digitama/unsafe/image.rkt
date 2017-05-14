#lang typed/racket/base

(provide (all-defined-out) bitmap-surface?)

(require "../types.rkt")
(require "font.rkt")
(require "source.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require "pangocairo.rkt")
  (require (submod "font.rkt" unsafe))

  (require racket/unsafe/ops)

  (define-values (A R G B) (if (system-big-endian?) (values 0 1 2 3) (values 3 2 1 0)))
  
  (define (λbitmap width height density λargb)
    (define-values (img cr w h) (make-cairo-image width height density))
    (define surface (cairo_get_target cr))
    (define buffer (cairo_image_surface_get_data surface))
    (define stride (cairo_image_surface_get_stride surface))
    (define total (unsafe-bytes-length buffer))
    (define W (unsafe-fxquotient stride 4))
    (define H (unsafe-fxquotient total stride))
    (for ([idx (in-range 0 total 4)])
      (define x (unsafe-fxquotient (unsafe-fxremainder idx stride) 4))
      (define y (unsafe-fxquotient idx stride))
      (define-values (a r g b) (λargb x y W H))
      (unsafe-bytes-set! buffer (unsafe-fx+ idx A) (argb->datum a))
      (unsafe-bytes-set! buffer (unsafe-fx+ idx R) (argb->datum r))
      (unsafe-bytes-set! buffer (unsafe-fx+ idx G) (argb->datum g))
      (unsafe-bytes-set! buffer (unsafe-fx+ idx B) (argb->datum b)))
    (cairo_destroy cr)
    img)
  
  (define (bitmap_blank width height density)
    (define-values (img cr w h) (make-cairo-image width height density))
    (cairo_destroy cr)
    img)

  (define (bitmap_pattern width height background density)
    (define-values (img cr w h) (make-cairo-image* width height background density))
    (cairo_destroy cr)
    img)
  
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
  (define (argb->datum v)
    (unsafe-fxmin
     (unsafe-fxmax
      (unsafe-fl->fx
       (unsafe-fl*
        (real->double-flonum v)
        255.0))
      #x00)
     #xFF))
  
  (define (bitmap_create_layout cr max-width max-height indent spacing wrap-mode ellipsize-mode)
    (define-values (smart-height smart-emode)
      (cond [(eq? ellipsize-mode 'PANGO_ELLIPSIZE_NONE) (values -1 ellipsize-mode)]
            [(or (eq? max-height +inf.0) (eq? max-height -inf.0)) (values -1 PANGO_ELLIPSIZE_NONE)]
            [(unsafe-fl< max-height 0.0) (values (unsafe-fl->fx max-height) ellipsize-mode)]
            [else (values (~size max-height) ellipsize-mode)]))
    (define context (the-context))
    (define layout (pango_layout_new context))
    (pango_layout_set_width layout (if (eq? max-width +inf.0) -1 (~size (unsafe-flmax max-width 0.0))))
    (pango_layout_set_height layout smart-height)
    (pango_layout_set_indent layout (~size indent))   ; nan? and infinite? are 0s
    (pango_layout_set_spacing layout (~size spacing)) ; pango knows the min spacing
    (pango_layout_set_wrap layout wrap-mode)
    (pango_layout_set_ellipsize layout smart-emode)
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

(define-type XYWH->ARGB (-> Nonnegative-Fixnum Nonnegative-Fixnum Positive-Fixnum Positive-Fixnum (Values Real Real Real Real)))

(require/typed/provide
 (submod "." unsafe)
 [λbitmap (-> Flonum Flonum Flonum XYWH->ARGB Bitmap)]
 [bitmap_blank (-> Flonum Flonum Flonum Bitmap)]
 [bitmap_pattern (-> Flonum Flonum Bitmap-Source Flonum Bitmap)]
 [bitmap_paragraph (-> String Flonum Flonum Flonum Flonum Integer Integer Font-Description (Listof Symbol)
                       Bitmap-Source Bitmap-Source Flonum Bitmap)])

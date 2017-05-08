#lang typed/racket/base

(provide Font-Description pango-create-font-desc pango_paragraph)
(provide get_font_metrics_lines get_font_metrics)

(require typed/racket/unsafe)
(require typed/racket/draw)

(module unsafe racket/base
  (provide (all-defined-out))
  (provide (rename-out [cpointer? font-description?]))

  (require racket/class)
  
  (require "ffi.rkt")
  (require (submod "image.rkt" unsafe))

  (define the-cairo (cairo-create-argb-image 1.0 1.0))
  (define &ink (make-PangoRectangle 0 0 0 0))
  (define &logical (make-PangoRectangle 0 0 0 0))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define get_font_metrics
    (lambda [font-desc]
      (define-values (baseline get-extent) (font-desc->get-extent font-desc))
      (define-values (xx xy xw xh xH) (get-extent "x"))
      (define-values (Ox Oy Ow Oh OH) (get-extent "O"))
      (define-values (0x 0y ch 0h 0H) (get-extent "0"))
      (define-values (wx wy ic wh wH) (get-extent "水"))
      
      (define ex  (unsafe-fx- baseline xy))
      (define cap (unsafe-fx- baseline Oy))
      (values (~metric ex) (~metric cap) (~metric ch) (~metric ic) (~metric wH))))

  (define get_font_metrics_lines
    (lambda [font-desc content]
      (define-values (baseline get-extent) (font-desc->get-extent font-desc))
      (define-values (xx meanline xw eh     eH) (get-extent "x"))
      (define-values (Ox capline  Ow Oh     OH) (get-extent "O"))
      (define-values (cx ascent   cw height cH) (get-extent content))
      
      (values (~metric ascent) (~metric capline) (~metric meanline) (~metric baseline)
              (~metric (unsafe-fx+ ascent height)))))

  (define (pango_paragraph words max-width max-height indent spacing wrap ellipsize color background font-desc density)
    (define layout (pango-reset-layout max-width max-height indent spacing wrap ellipsize))
    (pango_layout_set_font_description layout font-desc)
    (pango_layout_set_text layout words)

    (define-values (pango-width pango-height) (pango_layout_get_size layout))
    (define-values (flwidth flheight) (values (~metric pango-width) (unsafe-flmin (~metric pango-height) max-height)))
    (define-values (bmp cr draw-text?)
      (if (unsafe-fl<= flwidth max-width)
          (let-values ([(bmp cr) (make-cairo-image* flwidth flheight background density)])
            (values bmp cr #true))
          (let-values ([(w h) (and (pango_layout_set_text layout " ") (pango_layout_get_size layout))])
            (define draw-text? (unsafe-fl>= max-width (~metric w)))
            (define smart-height (if draw-text? flheight (unsafe-flmin (~metric h) flheight)))
            (define-values (bmp cr) (make-cairo-image* max-width smart-height background density))
            (values bmp cr draw-text?))))
    (when draw-text?
      (cairo-set-rgba cr color)
      (cairo_move_to cr 0 0)
      (pango_cairo_show_layout cr layout))        
    (cairo_destroy cr)
    bmp)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define the-layout
    (let ([&layout (box #false)])
      (lambda []
        (or (unbox &layout)
            (let ([layout (pango_cairo_create_layout the-cairo)])
              (set-box! &layout layout)
              layout)))))
  
  (define font-desc->get-extent
    (lambda [font-desc]
      (define layout (the-layout))

      (pango_layout_set_indent layout 0)
      (pango_layout_set_width layout -1)
      (pango_layout_set_height layout -1)
      (pango_layout_set_ellipsize layout 'PANGO_ELLIPSIZE_NONE)
      (pango_layout_set_font_description layout font-desc)

      (let ([baseline (pango_layout_get_baseline layout)])
        (values baseline (λ [hint-char] (~extent layout baseline hint-char))))))

  (define ~extent
    (lambda [layout baseline hint-char]
      (pango_layout_set_text layout hint-char)
      (pango_layout_get_extents layout &ink &logical)
      
      (define-values (x y width height) (~rectangle &ink))
      (define layout-height (PangoRectangle-height &logical))
      
      (values x y width height layout-height)))

  (define (pango-create-font-desc font-face font-size font-style font-weight)
    (define font-desc
      (let ([face-is-family? (regexp-match #rx"," font-face)])
        (cond [(not face-is-family?) (pango_font_description_from_string font-face)]
              [else (let ([desc (pango_font_description_new)])
                      (pango_font_description_set_family desc font-face)
                      desc)])))
    (unless (eq? font-style 'normal) (pango_font_description_set_style font-desc (~style font-style)))
    (unless (eq? font-weight 'normal) (pango_font_description_set_weight font-desc (~weight font-weight)))
    (pango_font_description_set_absolute_size font-desc (* font-size PANGO_SCALE))
    font-desc)
  
  (define (pango-reset-layout max-width max-height indent spacing wrap-mode ellipsize-mode)
    (define-values (smart-height smart-emode)
      (cond [(eq? ellipsize-mode 'PANGO_ELLIPSIZE_NONE) (values -1 ellipsize-mode)]
            [(or (eq? max-height +inf.0) (eq? max-height -inf.0)) (values -1 'PANGO_ELLIPSIZE_NONE)]
            [(unsafe-fl< max-height 0.0) (values (unsafe-fl->fx max-height) ellipsize-mode)]
            [else (values (~size max-height) ellipsize-mode)]))
    (define layout (the-layout))
    (pango_layout_set_width layout (if (eq? max-width +inf.0) -1 (~size (unsafe-flmax max-width 0.0))))
    (pango_layout_set_height layout smart-height)
    (pango_layout_set_indent layout (~size indent))   ; nan? and infinite? are 0s
    (pango_layout_set_spacing layout (~size spacing)) ; pango knows the min spacing
    (pango_layout_set_wrap layout wrap-mode)
    (pango_layout_set_ellipsize layout smart-emode)
    layout))

(unsafe-require/typed
 (submod "." unsafe)
 [#:opaque Font-Description font-description?]
 [pango-create-font-desc (-> String Real Symbol Symbol Font-Description)]
 [pango_paragraph (-> String Flonum Flonum Flonum Flonum Symbol Symbol (Instance Color%) (Instance Brush%) Font-Description Flonum
                      (Instance Bitmap%))])

(unsafe-require/typed
 (submod "." unsafe)
 [get_font_metrics_lines (-> Font-Description String (Values Flonum Flonum Flonum Flonum Flonum))]
 [get_font_metrics (-> Font-Description
                       (Values Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum))])


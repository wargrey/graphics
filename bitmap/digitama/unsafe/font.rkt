#lang typed/racket/base

(module unsafe racket/base
  (provide (all-defined-out))

  (require racket/unsafe/ops)
  (require racket/draw/unsafe/pango)
  (require racket/draw/unsafe/cairo)
  
  (define surface (cairo_image_surface_create CAIRO_FORMAT_ARGB32 1 1))
  (define cr (cairo_create surface))
  (define &layout (box #false))
  (define &ink (make-PangoRectangle 0 0 0 0))
  (define &logical (make-PangoRectangle 0 0 0 0))
  
  (define get_font_metrics
    (lambda [font-face font-size font-style font-weight]
      (define-values (font-desc baseline get-extent) (make-desc+extent font-face font-size font-style font-weight))
      (define-values (xx xy xw xh xH) (get-extent "x"))
      (define-values (Ox Oy Ow Oh OH) (get-extent "O"))
      (define-values (0x 0y ch 0h 0H) (get-extent "0"))
      (define-values (wx wy ic wh wH) (get-extent "水"))

      (pango_font_description_free font-desc)
      
      (define ex  (unsafe-fx- baseline xy))
      (define cap (unsafe-fx- baseline Oy))
      (values (~metric ex) (~metric cap) (~metric ch) (~metric ic) (~metric wH))))

  (define get_font_metrics_lines
    (lambda [font-face font-size font-style font-weight content]
      (define-values (font-desc baseline get-extent) (make-desc+extent font-face font-size font-style font-weight))
      (define-values (xx meanline xw eh     eH) (get-extent "x"))
      (define-values (Ox capline  Ow Oh     OH) (get-extent "O"))
      (define-values (cx ascent   cw height cH) (get-extent content))
      
      (pango_font_description_free font-desc)
      (values (~metric ascent) (~metric capline) (~metric meanline) (~metric baseline)
              (~metric (unsafe-fx+ ascent height)))))

  (define get-the-layout
    (lambda []
      (or (unbox &layout)
          (let ([layout (pango_cairo_create_layout cr)])
            (set-box! &layout layout)
            layout))))
  
  (define get_font~extent
    (lambda [font-face font-size font-style font-weight]
      (define-values (desc baseline get-extent) (make-desc+extent font-face font-size font-style font-weight))
      get-extent))
  
  (define make-desc+extent
    (lambda [font-face font-size font-style font-weight]
      (define layout (get-the-layout))
      (define font-desc (make-font-desc font-face font-size font-style font-weight))
      
      (pango_layout_set_font_description layout font-desc)

      (let ([baseline (pango_layout_get_baseline layout)])
        (values font-desc baseline (λ [hint-char] (~extent layout baseline hint-char))))))

  (define make-font-desc
    (lambda [font-face font-size font-style font-weight]
      (define font-desc
        (let ([face-is-family? (regexp-match #rx"," font-face)])
          (cond [(not face-is-family?) (pango_font_description_from_string font-face)]
                [else (let ([desc (pango_font_description_new)])
                        (pango_font_description_set_family desc font-face)
                        desc)])))
      (unless (eq? font-style 'normal) (pango_font_description_set_style font-desc (~style font-style)))
      (unless (eq? font-weight 'normal) (pango_font_description_set_weight font-desc (~weight font-weight)))
      (pango_font_description_set_absolute_size font-desc (* font-size PANGO_SCALE))
      font-desc))

  (define ~extent
    (lambda [layout baseline hint-char]
      (pango_layout_set_text layout hint-char)
      (pango_layout_get_extents layout &ink &logical)
      
      (define-values (x y width height) (~rectangle &ink))
      (define layout-height (PangoRectangle-height &logical))
      
      (values x y width height layout-height)))

  (define ~rectangle
    (lambda [&rect]
      (values (PangoRectangle-x &rect) (PangoRectangle-y &rect)
              (PangoRectangle-width &rect) (PangoRectangle-height &rect))))

  (define ~metric
    (lambda [val]
      (unsafe-fl/ (unsafe-fx->fl val)
                  (unsafe-fx->fl PANGO_SCALE))))
  
  (define (~style style)
    (case style
      [(italic) PANGO_STYLE_ITALIC]
      [(slant) PANGO_STYLE_OBLIQUE]
      [else PANGO_STYLE_NORMAL]))
  
  (define (~weight weight)
    (case weight
      [(light) PANGO_WEIGHT_LIGHT]
      [(bold) PANGO_WEIGHT_BOLD]
      [else PANGO_WEIGHT_MEDIUM])))

(require/typed/provide
 (submod "." unsafe)
 [get_font_metrics_lines (-> String Real Symbol Symbol String (Values Flonum Flonum Flonum Flonum Flonum))]
 [get_font_metrics (-> String Real Symbol Symbol
                       (Values Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum))])

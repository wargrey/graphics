#lang typed/racket/base

(module unsafe racket/base
  (provide (all-defined-out))

  (require racket/flonum)
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
      (define-values (xx xy xw ex  xa xd) (get-extent "x"))
      (define-values (Ox Oy Ow cap Oa Od) (get-extent "O"))
      (define-values (0x 0y ch 0h  0a 0d) (get-extent "0"))
      (define-values (wx wy ic wh  wa wd) (get-extent "水"))
      
      (pango_font_description_free font-desc)
      (values ex cap ch ic)))

  (define get_font_metrics_lines
    (lambda [font-face font-size font-style font-weight content]
      (define-values (font-desc baseline get-extent) (make-desc+extent font-face font-size font-style font-weight))
      (define-values (xx meanline xw eh     xa xd) (get-extent "x"))
      (define-values (Ox capline  Ow Oh     Oa Od) (get-extent "O"))
      (define-values (cx ascent   cw height ca cd) (get-extent content))
      
      (pango_font_description_free font-desc)
      (values ascent capline meanline baseline (+ ascent height))))
  
  (define make-desc+extent
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
      
      (define layout (or (unbox &layout) (and (set-box! &layout (pango_cairo_create_layout cr)) (unbox &layout))))
      (pango_layout_set_font_description layout font-desc)

      (let ([baseline (~metric (pango_layout_get_baseline layout))])
        (values font-desc baseline (λ [hint-char] (~extent layout baseline hint-char))))))

  (define ~extent
    (lambda [layout baseline hint-char]
      (pango_layout_set_text layout hint-char)
      (pango_layout_get_extents layout &ink &logical)
    
      (define-values (x y width height) (~rectangle &ink))

      (values x y width height
              (and 'ascent (unsafe-fl- baseline y))
              (and 'descent (unsafe-fl- (unsafe-fl+ y height) baseline)))))

  (define ~rectangle
    (lambda [&rect]
      (values (~metric (PangoRectangle-x &rect)) (~metric (PangoRectangle-y &rect))
              (~metric (PangoRectangle-width &rect)) (~metric (PangoRectangle-height &rect)))))

  (define ~metric
    (lambda [val]
      (unsafe-fl/ (real->double-flonum val)
                  (real->double-flonum PANGO_SCALE))))
  
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
 [get_font_metrics (-> String Real Symbol Symbol (Values Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum))]
 [get_font_metrics_lines (-> String Real Symbol Symbol String (Values Flonum Flonum Flonum Flonum Flonum))])

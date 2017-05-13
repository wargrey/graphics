#lang typed/racket/base

(provide (all-defined-out) Font-Description)

(require "source.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require "pangocairo.rkt")

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
      
      (values (~metric ascent) (~metric capline) (~metric meanline)
              (~metric baseline) (~metric (unsafe-fx+ ascent height)))))

  (define-values (pango-font-weights font-weight->integer integer->font-weight)
    (let ([font-weights (list '(thin . 100) '(ultralight . 200) '(light . 300) '(semilight . 350) '(book . 380)
                              '(normal . 400) '(medium . 500) '(semibold . 600)
                              '(bold . 700) '(ultrabold . 800) '(heavy . 900) '(ultraheavy . 1000))])
      (values (map unsafe-car font-weights)
              (λ [weight] (let ([kv (assq weight font-weights)])
                            (cond [(and kv) (unsafe-cdr kv)]
                                  [else (unsafe-cdr (assq 'normal font-weights))])))
              (λ [weight] (cond [(unsafe-fx< weight 150) 'thin]
                                [(unsafe-fx< weight 250) 'ultralight]
                                [(unsafe-fx< weight 325) 'light]
                                [(unsafe-fx< weight 365) 'semilight]
                                [(unsafe-fx< weight 390) 'book]
                                [(unsafe-fx< weight 450) 'normal]
                                [(unsafe-fx< weight 550) 'medium]
                                [(unsafe-fx< weight 650) 'bold]
                                [(unsafe-fx< weight 750) 'ultrabold]
                                [(unsafe-fx< weight 850) 'heavy]
                                [else 'ultraheavy])))))

  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define the-layout
    (let ([&layout (box #false)])
      (lambda []
        (or (unbox &layout)
            (let ([layout (time (pango_cairo_create_layout the-cairo))])
              (set-box! &layout layout)
              layout)))))
  
  (define font-desc->get-extent
    (lambda [font-desc]
      (define layout (the-layout))

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

  (define (bitmap_create_font_desc font-face font-size font-style font-weight font-stretch)
    (define font-desc
      (let ([face-is-family? (regexp-match #rx"," font-face)])
        (cond [(not face-is-family?) (pango_font_description_from_string font-face)]
              [else (let ([desc (pango_font_description_new)])
                      (pango_font_description_set_family desc font-face)
                      desc)])))
    (unless (eq? font-style 'normal) (pango_font_description_set_style font-desc (~style font-style)))
    (unless (eq? font-weight 'normal) (pango_font_description_set_weight font-desc (font-weight->integer font-weight)))
    (pango_font_description_set_stretch font-desc font-stretch)
    (pango_font_description_set_absolute_size font-desc (* font-size PANGO_SCALE))
    font-desc))

(require/typed/provide
 (submod "." unsafe)
 [pango-font-weights (Listof Symbol)]
 [font-weight->integer (-> Symbol Integer)]
 [integer->font-weight (-> Integer Symbol)]
 [bitmap_create_font_desc (-> String Real Symbol Symbol Symbol Font-Description)]
 [get_font_metrics_lines (-> Font-Description String (Values Flonum Flonum Flonum Flonum Flonum))]
 [get_font_metrics (-> Font-Description (Values Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                                                Nonnegative-Flonum Nonnegative-Flonum))])

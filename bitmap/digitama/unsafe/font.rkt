#lang typed/racket/base

(provide (all-defined-out))

(require "source.rkt")
(require "require.rkt")

(module unsafe racket/base
  (provide (all-defined-out))

  (require ffi/unsafe/atomic)
  
  (require "pangocairo.rkt")

  (define &ink (make-PangoRectangle 0 0 0 0))
  (define &logical (make-PangoRectangle 0 0 0 0))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (bitmap_create_font_desc font-face font-size weight style stretch)
    (define font-desc (pango_font_description_from_string font-face))
    (when weight (pango_font_description_set_weight font-desc weight))
    (when style (pango_font_description_set_style font-desc style))
    (when stretch (pango_font_description_set_stretch font-desc stretch))
    (pango_font_description_set_absolute_size font-desc (* font-size PANGO_SCALE))
    font-desc)
  
  (define (font_get_metrics font-desc)
    (start-atomic)
    (define-values (baseline get-extent) (font-desc->get-extent font-desc))
    (define-values (xy xw xh) (get-extent "x"))
    (define-values (Oy Ow Oh) (get-extent "O"))
    (define-values (0y ch 0h) (get-extent "0"))
    (define-values (wy ic wh wW wH) (get-extent "水" #true))
    (end-atomic)
    
    (define ex  (unsafe-fx- baseline xy))
    (define cap (unsafe-fx- baseline Oy))
    (list (cons 'em (~metric (pango_font_description_get_size font-desc)))
          (cons 'ex (~metric ex)) (cons 'cap (~metric cap))
          (cons 'ch (~metric ch)) (cons 'ic (~metric ic))
          (cons 'lh #|TODO: line height should be required|# (~metric wH))))

  (define (font_get_metrics_lines font-desc content)
    (start-atomic)
    (define-values (baseline get-extent) (font-desc->get-extent font-desc))
    (define-values (meanline xw eh)     (get-extent "x"))
    (define-values (capline  Ow Oh)     (get-extent "O"))
    (define-values (ascent   cw height) (get-extent content))
    (end-atomic)
    
    (values (~metric ascent) (~metric capline) (~metric meanline)
            (~metric baseline) (~metric (unsafe-fx+ ascent height))))

  (define (font_get_text_extent font-desc content)
    (start-atomic)
    (define-values (baseline get-extent) (font-desc->get-extent font-desc))
    (define-values (ascent _ height Width Height) (get-extent content #true))
    (end-atomic)
    
    (values (~metric Width) (~metric Height) (~metric (unsafe-fx- Height baseline))
            (~metric ascent) (~metric (unsafe-fx- Height (unsafe-fx+ ascent height)))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define system-ui
    (let ([facebase (make-hasheq)])
      (lambda [symfont deface]
        (hash-ref! facebase symfont
                   (λ _ (with-handlers ([exn? (λ _ deface)])
                          (let ([sysfont (dynamic-require 'racket/gui/base symfont)])
                            (send sysfont get-face))))))))
  
  (define the-layout
    (let ([&layout (box #false)])
      (lambda []
        (or (unbox &layout)
            (let ([layout (pango_cairo_create_layout the-cairo)])
              (set-box! &layout layout)
              layout)))))
  
  (define (font-desc->get-extent font-desc)
    (define layout (the-layout))
    (pango_layout_set_font_description layout font-desc)
    (let ([baseline (pango_layout_get_baseline layout)])
      (values baseline
              (λ [hint-char [full? #false]]
                (~extent layout baseline hint-char full?)))))

  (define (~extent layout baseline hint-char full?)
    (pango_layout_set_text layout hint-char)
    (pango_layout_get_extents layout &ink &logical)
    (cond [(not full?) (values (PangoRectangle-y &ink) (PangoRectangle-width &ink) (PangoRectangle-height &ink))]
          [else (values (PangoRectangle-y &ink) (PangoRectangle-width &ink) (PangoRectangle-height &ink)
                        (PangoRectangle-width &logical) (PangoRectangle-height &logical))])))

(unsafe/require/provide
 (submod "." unsafe)
 [system-ui (-> Symbol String String)]
 [bitmap_create_font_desc (-> String Real (Option Integer) (Option Integer) (Option Integer) Font-Description)]
 [font_get_metrics_lines (-> Font-Description String (Values Flonum Flonum Flonum Flonum Flonum))]
 [font_get_metrics (-> Font-Description (Listof (Pairof Symbol Nonnegative-Flonum)))]
 [font_get_text_extent (-> Font-Description String (Values Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum Flonum))])

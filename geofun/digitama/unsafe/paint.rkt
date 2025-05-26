#lang typed/racket/base

(require typed/racket/unsafe)

(require "../paint/self.rkt")

(require "source.rkt")
(require "typed/c.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require racket/math)
  (require racket/unsafe/ops)
  (require ffi/unsafe)
  
  (require "../paint/stroke.rkt")
  (require "cairo.rkt")
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define cairo-set-rgba
    (case-lambda
      [(cr src opacity)
       (cairo_set_source_rgba cr
                              (unsafe-struct*-ref src 0)
                              (unsafe-struct*-ref src 1)
                              (unsafe-struct*-ref src 2)
                              (unsafe-fl* (unsafe-struct*-ref src 3) opacity))]
      [(cr src opacity scale)
       (cairo_set_source_rgba cr
                              (unsafe-fl* (unsafe-struct*-ref src 0) scale)
                              (unsafe-fl* (unsafe-struct*-ref src 1) scale)
                              (unsafe-fl* (unsafe-struct*-ref src 2) scale)
                              (unsafe-fl* (unsafe-struct*-ref src 3) opacity))]))
  
  (define cairo-set-source
    (case-lambda
      [(cr src)
       (cond [(struct? src) #;(rgba? src) (cairo-set-rgba cr src 1.0)]
             [(eq? (cpointer-tag src) 'cairo_pattern_t) (cairo_set_source cr src)]
             [else (cairo_set_source_surface cr src 0.0 0.0)])]
      [(cr src scale)
       (cond [(struct? src) #;(rgba? src) (cairo-set-rgba cr src 1.0 scale)]
             [(eq? (cpointer-tag src) 'cairo_pattern_t) (cairo_set_source cr src)]
             [else (cairo_set_source_surface cr src 0.0 0.0)])]))

  (define cairo-set-stroke
    (case-lambda
      [(cr stroke)
       (cairo-set-rgba cr (unsafe-struct*-ref stroke 0) (unsafe-struct*-ref stroke 7))
       (cairo_set_line_width cr (unsafe-struct*-ref stroke 1))
       (cairo_set_line_cap cr (linecap->integer (unsafe-struct*-ref stroke 2)))
       (cairo_set_line_join cr (linejoin->integer (unsafe-struct*-ref stroke 3)))
       (cairo_set_dash cr (unsafe-struct*-ref stroke 5) (unsafe-struct*-ref stroke 6))
       (let ([ml (unsafe-struct*-ref stroke 4)])
         (unless (nan? ml) (cairo_set_miter_limit cr ml)))]
      [(cr stroke alt-width color-scale round?)
       (cairo-set-rgba cr (unsafe-struct*-ref stroke 0) (unsafe-struct*-ref stroke 7) color-scale)
       (cairo_set_line_width cr (or alt-width (unsafe-struct*-ref stroke 1)))
       (cairo_set_line_cap cr (if (not round?) (linecap->integer (unsafe-struct*-ref stroke 2)) CAIRO_LINE_CAP_ROUND))
       (cairo_set_line_join cr (if (not round?) (linejoin->integer (unsafe-struct*-ref stroke 3)) CAIRO_LINE_JOIN_ROUND))
       (cairo_set_dash cr (unsafe-struct*-ref stroke 5) (unsafe-struct*-ref stroke 6))]))

  (define cairo-set-source-as-stroke
    (lambda [cr src width color-scale round?]
      (cairo-set-source cr src color-scale)
      (cairo_set_line_width cr width)
      (when (or round?)
        (cairo_set_line_cap cr CAIRO_LINE_CAP_ROUND)
        (cairo_set_line_join cr CAIRO_LINE_JOIN_ROUND))))

  (define cairo-set-thickline-stroke
    (lambda [cr stroke src width color-scale round?]
      (cond [(or stroke) (cairo-set-stroke cr stroke width 1.0 round?)]
            [(or src) (cairo-set-source-as-stroke cr src width color-scale round?)])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define cairo-render-with-stroke
    (case-lambda
      [(cr stroke)
       (cairo-set-stroke cr stroke)
       (cairo_stroke_preserve cr)]
      [(cr stroke alt-width color-scale round?)
       (cairo-set-stroke cr stroke alt-width color-scale round?)
       (cairo_stroke_preserve cr)]))

  (define cairo-render-with-source-as-stroke
    (lambda [cr src width color-scale round?]
      (cairo-set-source-as-stroke cr src width color-scale round?)
      (cairo_stroke_preserve cr)))

  (define cairo-render-with-fill
    (case-lambda
      [(cr pattern)
       (cairo-set-source cr pattern)
       (cairo_fill_preserve cr)]
      [(cr pattern rule)
       (cairo-set-fill-rule cr rule)
       (cairo-render-with-fill cr pattern)]))
  
  (define cairo-render-background
    (lambda [cr bg x y w h]
      (when (and bg)
        (cairo_save cr)
        (cairo-clip cr x y w h)
        (cairo-set-source cr bg)
        (cairo_paint cr)
        (cairo_restore cr))))

  (define cairo-render
    (case-lambda
      [(cr border)
       (unless (not border)
         (cairo-render-with-stroke cr border))]
      [(cr border pattern)
       (unless (not pattern)
         (cairo-render-with-fill cr pattern))
       (unless (not border)
         (cairo-render-with-stroke cr border))]
      [(cr border pattern rule)
       (unless (not pattern)
         (cairo-render-with-fill cr pattern rule))
       (unless (not border)
         (cairo-render-with-stroke cr border))]))
  
  (define cairo-set-fill-rule
    (lambda [cr fill-rule]
      (if (eq? fill-rule 'winding)
          (cairo_set_fill_rule cr CAIRO_FILL_RULE_WINDING)
          (cairo_set_fill_rule cr CAIRO_FILL_RULE_EVEN_ODD))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define cairo-composite
    (lambda [cr src dest-x dest-y dest-width dest-height filter sx sy]
      (cairo_save cr)
      (cairo_translate cr dest-x dest-y)
      (cairo_rectangle cr 0.0 0.0 dest-width dest-height)
      (cairo_scale cr sx sy)
      (cairo_set_source_surface cr src 0.0 0.0)
      (cairo_pattern_set_filter (cairo_get_source cr) filter)
      (cairo_fill cr)
      (cairo_restore cr)))

  (define cairo-composite!
    (lambda [master cr draw! dest-x dest-y dest-width dest-height]
      (cairo_save cr)
      (cairo_new_path cr)
      (draw! master cr dest-x dest-y dest-width dest-height)
      (cairo_restore cr)))

  (define cairo-clip
    (lambda [cr x y width height]
      (cairo_rectangle cr x y width height)
      (cairo_clip cr)))
  
  (define cairo-mask
    (lambda [cr src dest-x dest-y dest-width dest-height density]
      (define 1/density (unsafe-fl/ 1.0 density))
      
      (cairo_save cr)
      (cairo_translate cr dest-x dest-y)
      (cairo_scale cr 1/density 1/density) ; order matters
      (cairo_mask_surface cr src 0.0 0.0)
      (cairo_restore cr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [cairo-render-background (-> Cairo-Ctx (Option Fill-Source) Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Void)]
 [cairo-render-with-source-as-stroke (-> Cairo-Ctx Fill-Source Flonum Flonum Boolean Void)]
 [cairo-set-fill-rule (-> Cairo-Ctx Symbol Void)]
 [cairo-clip (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Void)]

 [cairo-set-source-as-stroke (-> Cairo-Ctx Fill-Source Flonum Flonum Boolean Void)]
 [cairo-set-thickline-stroke (-> Cairo-Ctx (Option Stroke) (Option Fill-Source) Flonum Flonum Boolean Void)]
 [cairo-set-source (case-> [Cairo-Ctx Fill-Source -> Void]
                           [Cairo-Ctx Fill-Source Flonum -> Void])]
 
 [cairo-set-stroke (case-> [Cairo-Ctx Stroke -> Void]
                           [Cairo-Ctx Stroke (Option Flonum) Flonum Boolean -> Void])]
 
 [cairo-render-with-stroke (case-> [Cairo-Ctx Stroke -> Void]
                                   [Cairo-Ctx Stroke (Option Flonum) Flonum Boolean -> Void])]
 
 [cairo-composite
  (-> Cairo-Ctx Cairo-Surface
      Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
      Byte Flonum Flonum
      Void)]
 
 [cairo-composite!
  (All (Master)
       (-> Master Cairo-Ctx (Cairo-Surface-Draw! Master)
           Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
           Void))]

 [cairo-render-with-fill
  (case-> [Cairo-Ctx Fill-Source -> Void]
          [Cairo-Ctx Fill-Source Fill-Rule -> Void])]

 [cairo-render
  (case-> [Cairo-Ctx (Option Stroke) -> Void]
          [Cairo-Ctx (Option Stroke) (Option Fill-Source) -> Void]
          [Cairo-Ctx (Option Stroke) (Option Fill-Source) Fill-Rule -> Void])])

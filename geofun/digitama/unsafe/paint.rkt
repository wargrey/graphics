#lang typed/racket/base

(require typed/racket/unsafe)
(provide (all-from-out "../paint/self.rkt"))

(require "../paint/self.rkt")
(require "typed/c.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require racket/math)
  (require racket/unsafe/ops)
  
  (require "../paint/stroke.rkt")
  (require "cairo.rkt")
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define cairo-set-fill-rule
    (lambda [cr fill-rule]
      (if (eq? fill-rule 'even-odd)
          (cairo_set_fill_rule cr CAIRO_FILL_RULE_EVEN_ODD)
          (cairo_set_fill_rule cr CAIRO_FILL_RULE_WINDING))))
  
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

  (define cairo-select-source
    ;;; TODO: deal with patterns
    (case-lambda
      [(cr brush)
       (define pattern (unsafe-struct*-ref brush 1))

       (cond [(not pattern) (cairo-set-rgba cr (unsafe-struct*-ref brush 0) (unsafe-struct*-ref brush 2))]
             [else (cairo_set_source_surface cr pattern 0.0 0.0)])]
      [(cr brush color-scale)
       (define pattern (unsafe-struct*-ref brush 1))
       
       (cond [(not pattern) (cairo-set-rgba cr (unsafe-struct*-ref brush 0) (unsafe-struct*-ref brush 2) color-scale)]
             [else (cairo_set_source_surface cr pattern 0.0 0.0)])]))
  
  (define cairo-set-source
    (case-lambda
      [(cr brush)
       (cairo-select-source cr brush)
       (cairo-set-fill-rule cr (unsafe-struct*-ref brush 3))]
      [(cr brush color-scale)
       (cairo-select-source cr brush color-scale)
       (cairo-set-fill-rule cr (unsafe-struct*-ref brush 3))]))

  (define cairo-set-evenodd-source
    (case-lambda
      [(cr brush)
       (cairo-select-source cr brush)
       (cairo_set_fill_rule cr CAIRO_FILL_RULE_EVEN_ODD)]
      [(cr brush color-scale)
       (cairo-select-source cr brush color-scale)
       (cairo_set_fill_rule cr CAIRO_FILL_RULE_EVEN_ODD)]))

  (define cairo-set-stroke
    (case-lambda
      [(cr stroke)
       (cairo-set-rgba cr (unsafe-struct*-ref stroke 0) (unsafe-struct*-ref stroke 7))
       (cairo_set_line_width cr (unsafe-struct*-ref stroke 1))
       (cairo_set_line_cap cr (unsafe-struct*-ref stroke 2))
       (cairo_set_line_join cr (unsafe-struct*-ref stroke 3))
       (cairo_set_miter_limit cr (unsafe-struct*-ref stroke 4))
       (cairo_set_dash cr (unsafe-struct*-ref stroke 5) (unsafe-struct*-ref stroke 6))]
      [(cr stroke alt-width color-scale round?)
       (cairo-set-rgba cr (unsafe-struct*-ref stroke 0) (unsafe-struct*-ref stroke 7) color-scale)
       (cairo_set_line_width cr (or alt-width (unsafe-struct*-ref stroke 1)))
       (cairo_set_dash cr (unsafe-struct*-ref stroke 5) (unsafe-struct*-ref stroke 6))
       (if (or round?)
           (let ()
             (cairo_set_line_cap cr CAIRO_LINE_CAP_ROUND)
             (cairo_set_line_join cr CAIRO_LINE_JOIN_ROUND))
           (let ()
             (cairo_set_line_cap cr (unsafe-struct*-ref stroke 2))
             (cairo_set_line_join cr (unsafe-struct*-ref stroke 3))
             (cairo_set_miter_limit cr (unsafe-struct*-ref stroke 4))))]))

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
      [(cr brush)
       (cairo-set-source cr brush)
       (cairo_fill_preserve cr)]))

  (define cairo-render/evenodd
    (case-lambda
      [(cr border brush)
       (unless (not brush)
         (cairo-set-evenodd-source cr brush)
         (cairo_fill_preserve cr))
       (unless (not border)
         (cairo-render-with-stroke cr border))]))
  
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
      [(cr border brush)
       (unless (not brush)
         (cairo-render-with-fill cr brush))
       (unless (not border)
         (cairo-render-with-stroke cr border))]))
  
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
 [cairo-render (->* (Cairo-Ctx (Option Pen)) ((Option Brush)) Void)]
 [cairo-render/evenodd (-> Cairo-Ctx (Option Pen) (Option Brush) Void)]

 [cairo-render-background (-> Cairo-Ctx (Option Brush) Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Void)]
 [cairo-render-with-source-as-stroke (-> Cairo-Ctx Brush Flonum Flonum Boolean Void)]
 [cairo-render-with-fill (-> Cairo-Ctx Brush Void)]
 [cairo-clip (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Void)]

 [cairo-set-source-as-stroke (-> Cairo-Ctx Brush Flonum Flonum Boolean Void)]
 [cairo-set-thickline-stroke (-> Cairo-Ctx (Option Pen) (Option Brush) Flonum Flonum Boolean Void)]
 [cairo-set-source (->* (Cairo-Ctx Brush) (Flonum) Void)]
 [cairo-set-stroke (->* (Cairo-Ctx Pen) ((Option Flonum) Flonum Boolean) Void)]
 [cairo-render-with-stroke (->* (Cairo-Ctx Pen) ((Option Flonum) Flonum Boolean) Void)]
 
 [cairo-composite
  (-> Cairo-Ctx Cairo-Surface
      Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
      Byte Flonum Flonum
      Void)]
 
 [cairo-composite!
  (All (Master)
       (-> Master Cairo-Ctx (Cairo-Surface-Draw! Master)
           Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
           Void))])

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
  (require "../surface/image.rkt")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (dc_toy_text create-surface text font-desc stroke-source fill-source bgsource density)
    (define-values (face size style weight) (cairo-font-description-values font-desc))
    (define-values (width height baseline) (cairo-toy-text-matrics face size style weight text))
    (define-values (flx flwidth flheight flbaseline) (dc-toy-text-adjust-size width height baseline stroke-source))
    (define-values (sfc cr) (create-surface flwidth flheight density #true))
    
    (cairo-render-background cr bgsource)

    (cairo_select_font_face cr face style weight)
    (cairo_set_font_size cr size)
    
    (cairo_move_to cr flx flbaseline)
    (cairo_text_path cr text)
    (cairo-render cr stroke-source fill-source)
    
    (cairo_destroy cr)

    sfc)

  (define (dc_toy_text_size text font-desc stroke)
    (define-values (face size style weight) (cairo-font-description-values font-desc))
    (define-values (width height baseline) (cairo-toy-text-matrics face size style weight text))
    (define-values (x flwidth flheight flbaseline) (dc-toy-text-adjust-size width height baseline stroke))

    (values flwidth flheight))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (cairo-font-description-values font-desc)
    (values (pango_font_description_get_family font-desc)
            (~metric (pango_font_description_get_size font-desc))
            (pango_font_description_get_style font-desc) ; TODO: swap the italic and oblique
            (if (> (pango_font_description_get_weight font-desc) 400) 1 0)))

  (define (dc-toy-text-adjust-size width height baseline stroke)
    (values 0.0 width height baseline))
  
  (define (cairo-toy-text-matrics face size style weight utf8)
    (define &extents (make-cairo_text_extents_t 0.0 0.0 0.0 0.0 0.0 0.0))
    
    (cairo_select_font_face the-image-cairo face style weight)
    (cairo_set_font_size the-image-cairo size)
    (cairo_text_extents the-image-cairo utf8 &extents)
    
    (values (unsafe-fl+ (cairo_text_extents_t-width &extents) ; === x_advance
               (unsafe-fl* 2.0 (cairo_text_extents_t-x_bearing &extents)))
            (cairo_text_extents_t-height &extents)
            (unsafe-fl- (cairo_text_extents_t-y_bearing &extents)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [dc_toy_text_size (-> String Font-Description (Option Paint) (Values Nonnegative-Flonum Nonnegative-Flonum))]
 
 [dc_toy_text
  (All (S) (-> (Cairo-Surface-Create S)
               String Font-Description (Option Paint) (Option Fill-Source) (Option Fill-Source)
               Flonum S))])

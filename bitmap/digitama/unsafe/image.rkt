#lang typed/racket/base

(provide (all-defined-out))
(provide bitmap_paragraph)

(require typed/racket/unsafe)
(require typed/racket/draw)

(require "../types.rkt")
(require "font.rkt")
(require "background.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require "ffi.rkt")
  (require (submod "font.rkt" unsafe))

  (require racket/flonum)
  (require racket/unsafe/ops)
  
  (define (bitmap_paragraph words max-width max-height indent spacing wrap ellipsize font-desc color background density)
    (define layout (pango-create-layout the-cairo max-width max-height indent spacing wrap ellipsize))
    (pango_layout_set_font_description layout font-desc)
    (pango_layout_set_text layout words)

    (define-values (pango-width pango-height) (pango_layout_get_size layout))
    (define-values (flwidth flheight) (values (~metric pango-width) (unsafe-flmin (~metric pango-height) max-height)))
    (define-values (bmp cr draw-text?)
      (if (unsafe-fl<= flwidth max-width)
          (let-values ([(bmp cr w h) (make-cairo-image* flwidth flheight background density)])
            (values bmp cr #true))
          (let-values ([(w h) (and (pango_layout_set_text layout " ") (pango_layout_get_size layout))])
            (define draw-text? (unsafe-fl>= max-width (~metric w)))
            (define smart-height (if draw-text? flheight (unsafe-flmin (~metric h) flheight)))
            (define-values (bmp cr w h) (make-cairo-image* max-width smart-height background density))
            (values bmp cr draw-text?))))
    (when draw-text?
      (cairo-set-rgba cr color)
      (cairo_move_to cr 0 0)
      (pango_cairo_show_layout cr layout))        
    (cairo_destroy cr)
    bmp)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (pango-create-layout cr max-width max-height indent spacing wrap-mode ellipsize-mode)
    (define-values (smart-height smart-emode)
      (cond [(eq? ellipsize-mode 'PANGO_ELLIPSIZE_NONE) (values -1 ellipsize-mode)]
            [(or (eq? max-height +inf.0) (eq? max-height -inf.0)) (values -1 'PANGO_ELLIPSIZE_NONE)]
            [(unsafe-fl< max-height 0.0) (values (unsafe-fl->fx max-height) ellipsize-mode)]
            [else (values (~size max-height) ellipsize-mode)]))
    (define layout (pango_cairo_create_layout cr))
    (pango_layout_set_width layout (if (eq? max-width +inf.0) -1 (~size (unsafe-flmax max-width 0.0))))
    (pango_layout_set_height layout smart-height)
    (pango_layout_set_indent layout (~size indent))   ; nan? and infinite? are 0s
    (pango_layout_set_spacing layout (~size spacing)) ; pango knows the min spacing
    (pango_layout_set_wrap layout wrap-mode)
    (pango_layout_set_ellipsize layout smart-emode)
    layout))

(unsafe-require/typed
 (submod "." unsafe)
 [bitmap_paragraph (-> String Flonum Flonum Flonum Flonum Symbol Symbol Font-Description FlVector Brush-Pattern* Flonum Bitmap)])

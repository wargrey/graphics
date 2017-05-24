#lang racket/base

(provide (all-defined-out))
(provide (all-from-out ffi/unsafe racket/unsafe/ops))
(provide (all-from-out racket/draw/unsafe/pango racket/draw/unsafe/cairo))

(require ffi/unsafe)
(require ffi/unsafe/define)

(require racket/unsafe/ops)
(require racket/draw/unsafe/pango)
(require racket/draw/unsafe/cairo)
(require racket/draw/private/utils)

(require racket/class)
(require racket/draw)
(require racket/flonum)

(define-syntax-rule (_pfun spec ...) (_fun #:lock-name "cairo-pango-lock" spec ...))

(define pango-lib
  (case (system-type)
    [(unix) (ffi-lib "libpango-1.0" '("0" ""))]
    [(macosx) (ffi-lib "libpango-1.0.0.dylib")]
    [(windows) (ffi-lib "libpango-1.0-0.dll")]))

(define-ffi-definer define-pango pango-lib #:provide provide)

(define PangoContext (_cpointer 'PangoContext))
(define PangoLayout (_cpointer 'PangoLayout))
(define PangoFontDescription (_cpointer 'PangoFontDescription))
(define PangoAttribute (_cpointer 'PangoAttribute))

(define-enum 0 PANGO_WRAP_WORD PANGO_WRAP_CHAR PANGO_WRAP_WORD_CHAR)
(define-enum 0 PANGO_ELLIPSIZE_NONE PANGO_ELLIPSIZE_START PANGO_ELLIPSIZE_MIDDLE PANGO_ELLIPSIZE_END)

(define _gunichar (make-ctype _uint32 char->integer integer->char))

(define _font-stretch
  (_enum '(ultra-condensed extra-condensed condensed semi-condensed normal semi-expanded expanded extra-expanded ultra-expanded)))

(define-pango pango_context_set_font_description (_pfun PangoContext PangoFontDescription -> _void))
(define-pango pango_font_description_set_stretch (_pfun PangoFontDescription _font-stretch -> _void))
(define-pango pango_attr_strikethrough_new (_pfun _bool -> PangoAttribute))
(define-pango pango_attr_underline_new (_pfun _int -> PangoAttribute))

(define-pango pango_layout_get_size (_pfun PangoLayout [w : (_ptr o _int)] [h : (_ptr o _int)] -> _void -> (values w h)))

(define-pango pango_layout_set_indent (_pfun PangoLayout _int -> _void))
(define-pango pango_layout_set_spacing (_pfun PangoLayout _int -> _void))
(define-pango pango_layout_set_width (_pfun PangoLayout _int -> _void))
(define-pango pango_layout_set_height (_pfun PangoLayout _int -> _void))
(define-pango pango_layout_set_wrap (_pfun PangoLayout _int -> _void))
(define-pango pango_layout_set_ellipsize (_pfun PangoLayout _int -> _void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-values (A R G B) (if (system-big-endian?) (values 0 1 2 3) (values 3 2 1 0)))

(define cairo-create-argb-image
  (lambda [flwidth flheight [density 1.0]]
    (define surface
      (cairo_image_surface_create CAIRO_FORMAT_ARGB32
                                  (unsafe-fxmax (unsafe-fl->fx (unsafe-fl* flwidth density)) 1)
                                  (unsafe-fxmax (unsafe-fl->fx (unsafe-fl* flheight density)) 1)))
    
    (define cr (cairo_create surface))
    (unless (unsafe-fl= density 1.0) (cairo_scale cr density density))
    (cairo_surface_destroy surface)
    cr))

(define make-cairo-image
  (lambda [flwidth flheight [density 1.0]]
    (define width (unsafe-fxmax (unsafe-fl->fx flwidth) 1))
    (define height (unsafe-fxmax (unsafe-fl->fx flheight) 1))
    (define img (make-bitmap width height #:backing-scale density))
    (define cr (cairo_create (send img get-handle)))
    (unless (unsafe-fl= density 1.0) (cairo_scale cr density density))
    (values img cr width height)))

(define make-cairo-image*
  (lambda [flwidth flheight background [density 1.0]]
    (define-values (img cr width height) (make-cairo-image flwidth flheight density))
    (cairo-set-source cr background)
    (cairo_paint cr)
    (values img cr width height)))

(define cairo-set-source
  (lambda [cr src]
    (cond [(flvector? src)
           (cairo_set_source_rgba cr
                                  (unsafe-flvector-ref src 0)
                                  (unsafe-flvector-ref src 1)
                                  (unsafe-flvector-ref src 2)
                                  (unsafe-flvector-ref src 3))]
          [(cpointer? src)
           (case (cpointer-tag src)
             [(cairo_pattern_t) (cairo_set_source cr src)]
             [(cairo_surface_t) (cairo_set_source_surface cr src 0.0 0.0)]
             [else (cairo-warn-message src "unrecognized source pointer: ~a" (cpointer-tag src))])]
          [else (cairo-warn-message src "unrecognized source type: ~a" src)])))

(define cairo-image->bitmap
  (lambda [cr flwidth flheight [density 1.0]]
    (define-values (width height) (values (unsafe-fxmax (unsafe-fl->fx flwidth) 1) (unsafe-fxmax (unsafe-fl->fx flheight) 1)))
    (define img (make-object bitmap% width height #false #true density))
    (define-values (src dest) (values (cairo_get_target cr) (send img get-handle)))
    (define-values (src-data dest-data) (values (cairo_image_surface_get_data src) (cairo_image_surface_get_data dest)))
    (define-values (src-step dest-step) (values (cairo_image_surface_get_stride src) (cairo_image_surface_get_stride dest)))
    (define-values (total safe-step) (values (unsafe-bytes-length dest-data) (unsafe-fxmin src-step dest-step)))
    (cond [(eq? src-step dest-step) (memcpy dest-data src-data total _byte)]
          [else (let rowcpy ([srcoff 0] [destoff 0])
                  (define destnext (unsafe-fx+ destoff dest-step))
                  (memcpy dest-data destoff src-data srcoff safe-step _byte)
                  (when (< destnext total) (rowcpy (unsafe-fx+ srcoff src-step) destnext)))])
    (cairo_surface_mark_dirty dest)
    img))

(define cairo-warn-message
  (lambda [src msg . args]
    (define message (if (null? args) msg (apply format msg args)))
    (log-message (current-logger) 'warning 'exn:css:bitmap message src)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ~size
  (lambda [size]
    (unsafe-fx* (unsafe-fl->fx size)
                PANGO_SCALE)))

(define ~metric
  (lambda [val]
    (unsafe-fl/ (unsafe-fx->fl val)
                (unsafe-fx->fl PANGO_SCALE))))

(define ~rectangle
  (lambda [&rect]
    (values (PangoRectangle-x &rect) (PangoRectangle-y &rect)
            (PangoRectangle-width &rect) (PangoRectangle-height &rect))))

(define (~style style)
  (case style
    [(italic) PANGO_STYLE_ITALIC]
    [(slant) PANGO_STYLE_OBLIQUE]
    [else PANGO_STYLE_NORMAL]))

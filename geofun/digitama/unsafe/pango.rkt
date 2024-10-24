#lang racket/base

(provide (all-defined-out))
(provide (all-from-out racket/draw/unsafe/pango))

(require ffi/unsafe)
(require ffi/unsafe/define)

(require racket/unsafe/ops)
(require racket/draw/unsafe/pango)
(require racket/draw/unsafe/cairo)

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax-rule (_pfun spec ...) (_fun #:lock-name (or cairo-lock-name "pango-lock") spec ...))

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

(define-pango pango_version_string (_pfun -> _string))

(define-pango pango_context_set_font_description (_pfun PangoContext PangoFontDescription -> _void))
(define-pango pango_font_description_get_size (_pfun PangoFontDescription -> _int))
(define-pango pango_font_description_get_style (_pfun PangoFontDescription -> _int))
(define-pango pango_font_description_get_weight (_pfun PangoFontDescription -> _int))
(define-pango pango_font_description_get_stretch (_pfun PangoFontDescription -> _int))
(define-pango pango_font_description_get_variant (_pfun PangoFontDescription -> _int))
(define-pango pango_font_description_set_stretch (_pfun PangoFontDescription _int -> _void))
(define-pango pango_font_description_set_variant (_pfun PangoFontDescription _int -> _void))
(define-pango pango_attr_strikethrough_new (_pfun _bool -> PangoAttribute))
(define-pango pango_attr_underline_new (_pfun _int -> PangoAttribute))
;(define-pango pango_attr_overline_new (_pfun _int -> PangoAttribute)) ; since Pango 1.46

(define-pango pango_layout_get_size (_pfun PangoLayout [w : (_ptr o _int)] [h : (_ptr o _int)] -> _void -> (values w h)))
(define-pango pango_layout_get_line_count (_pfun PangoLayout -> _int))
(define-pango pango_layout_set_indent (_pfun PangoLayout _int -> _void))
(define-pango pango_layout_set_spacing (_pfun PangoLayout _int -> _void))
(define-pango pango_layout_set_width (_pfun PangoLayout _int -> _void))
(define-pango pango_layout_set_height (_pfun PangoLayout _int -> _void))
(define-pango pango_layout_set_wrap (_pfun PangoLayout _int -> _void))
(define-pango pango_layout_set_ellipsize (_pfun PangoLayout _int -> _void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ~pango-size
  (lambda [size]
    (unsafe-fx* (unsafe-fl->fx (unsafe-flceiling size))
                PANGO_SCALE)))

(define ~pango-metric
  (lambda [val]
    (unsafe-fl/ (unsafe-fx->fl val)
                (unsafe-fx->fl PANGO_SCALE))))

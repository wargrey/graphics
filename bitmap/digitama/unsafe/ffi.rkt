#lang racket/base

(provide (all-defined-out))
(provide (all-from-out ffi/unsafe racket/unsafe/ops))
(provide (all-from-out racket/draw/unsafe/pango racket/draw/unsafe/cairo))

(require ffi/unsafe)
(require ffi/unsafe/define)

(require racket/unsafe/ops)
(require racket/draw/unsafe/pango)
(require racket/draw/unsafe/cairo)

(define PangoLayout (_cpointer 'PangoLayout))
(define pango-lib
  (case (system-type)
    [(unix) (ffi-lib "libpango-1.0" '("0" ""))]
    [(macosx) (ffi-lib "libpango-1.0.0.dylib")]
    [(windows) (ffi-lib "libpango-1.0-0.dll")]))

(define-ffi-definer define-pango pango-lib #:provide provide)

(define _pango_wrap_mode (_enum '(PANGO_WRAP_WORD PANGO_WRAP_CHAR PANGO_WRAP_WORD_CHAR)))
(define _pango_ellipsize_mode (_enum '(PANGO_ELLIPSIZE_NONE PANGO_ELLIPSIZE_START PANGO_ELLIPSIZE_MIDDLE PANGO_ELLIPSIZE_END)))

(define-pango pango_layout_get_size
  (_fun #:lock-name "cairo-pango-lock"
        PangoLayout [w : (_ptr o _int)] [h : (_ptr o _int)]
        -> _void
        -> (values w h)))

(define-pango pango_layout_set_indent (_fun #:lock-name "cairo-pango-lock" PangoLayout _int -> _void))
(define-pango pango_layout_set_spacing (_fun #:lock-name "cairo-pango-lock" PangoLayout _int -> _void))
(define-pango pango_layout_set_width (_fun #:lock-name "cairo-pango-lock" PangoLayout _int -> _void))
(define-pango pango_layout_set_height (_fun #:lock-name "cairo-pango-lock" PangoLayout _int -> _void))
(define-pango pango_layout_set_wrap (_fun #:lock-name "cairo-pango-lock" PangoLayout _pango_wrap_mode -> _void))
(define-pango pango_layout_set_ellipsize (_fun #:lock-name "cairo-pango-lock" PangoLayout _pango_ellipsize_mode -> _void))

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

(define (~weight weight)
  (case weight
    [(light) PANGO_WEIGHT_LIGHT]
    [(bold) PANGO_WEIGHT_BOLD]
    [else PANGO_WEIGHT_MEDIUM]))

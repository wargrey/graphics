#lang racket/base

(provide (all-defined-out))
(provide (all-from-out racket/draw/unsafe/pango))

(require ffi/unsafe)
(require ffi/unsafe/define)
(require ffi/unsafe/alloc)

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
(define PangoFont (_cpointer 'PangoFont))
(define PangoFontDescription (_cpointer 'PangoFontDescription))
(define PangoAttribute (_cpointer 'PangoAttribute))
(define PangoAttrList (_cpointer/null 'PangoAttrList))
(define PangoAttrIterator (_cpointer 'PangoAttrIterator))

(define-pango pango_version_string (_pfun -> _string))

(define-pango pango_context_set_font_description (_pfun PangoContext PangoFontDescription -> _void))
(define-pango pango_font_description_get_size (_pfun PangoFontDescription -> _int))
(define-pango pango_font_description_get_style (_pfun PangoFontDescription -> _int))
(define-pango pango_font_description_get_weight (_pfun PangoFontDescription -> _int))
(define-pango pango_font_description_get_stretch (_pfun PangoFontDescription -> _int))
(define-pango pango_font_description_get_variant (_pfun PangoFontDescription -> _int))
(define-pango pango_font_description_set_stretch (_pfun PangoFontDescription _int -> _void))
(define-pango pango_font_description_set_variant (_pfun PangoFontDescription _int -> _void))

(define-pango pango_attr_strikethrough_new (_pfun _bool -> PangoAttribute) #:wrap (allocator pango_attribute_destroy))
(define-pango pango_attr_underline_new (_pfun _int -> PangoAttribute) #:wrap (allocator pango_attribute_destroy))
;(define-pango pango_attr_overline_new (_pfun _int -> PangoAttribute) #:wrap (allocator pango_attribute_destroy)) ; since Pango 1.46

(define-pango pango_layout_get_size (_pfun PangoLayout [w : (_ptr o _int)] [h : (_ptr o _int)] -> _void -> (values w h)))
(define-pango pango_layout_get_line_count (_pfun PangoLayout -> _int))
(define-pango pango_layout_set_indent (_pfun PangoLayout _int -> _void))
(define-pango pango_layout_set_spacing (_pfun PangoLayout _int -> _void))
(define-pango pango_layout_set_width (_pfun PangoLayout _int -> _void))
(define-pango pango_layout_set_height (_pfun PangoLayout _int -> _void))
(define-pango pango_layout_set_wrap (_pfun PangoLayout _int -> _void))
(define-pango pango_layout_set_ellipsize (_pfun PangoLayout _int -> _void))
(define-pango pango_layout_set_alignment (_pfun PangoLayout _int -> _void))

;;; The returned attrlist is owned by the layout, so don't need to release it
(define-pango pango_layout_get_attributes (_pfun PangoLayout -> PangoAttrList))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-cstruct _GError
  ([domain  _uint32]
   [code    _int]
   [message _bytes]))

(define-pango g_error_free (_pfun _GError-pointer -> _void)
  #:wrap (deallocator))

(define-pango pango_attr_iterator_destroy (_pfun PangoAttrIterator -> _void)
  #:wrap (deallocator))

(define-pango pango_attr_list_get_iterator (_pfun PangoAttrList -> PangoAttrIterator)
  #:wrap (allocator pango_attr_iterator_destroy))

(define-pango pango_attr_iterator_next (_pfun PangoAttrIterator -> _bool))

(define-pango pango_layout_set_markup
  (_pfun [layout : PangoLayout]
         [markup-text : _bytes]
         [length : _int = (bytes-length markup-text)]
         -> _void))

(define-pango pango_parse_markup
  (_pfun [markup-text : _bytes]
         [length : _int = (bytes-length markup-text)]
         [accelerator_marker : _byte = 0]
         [attr-list : (_ptr io PangoAttrList) = #false]
         [text : (_ptr io _pointer) = #false]
         [accelerator : (_or-null _pointer) = #false]
         [error : (_ptr io _pointer) = #false]
         -> [okay? : _bool]
         -> (if okay?
                (let ([plaintext (bytes->string/utf-8 (cast text _pointer _bytes))])
                  (register-finalizer attr-list pango_attr_list_unref)
                  (g_free text)
                  (cons attr-list plaintext))
                (let* ([e (cast error _pointer _GError-pointer)]
                       [errmsg (GError-message e)])
                  (g_error_free e)
                  errmsg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ~pango-size
  (lambda [size]
    (unsafe-fx* (unsafe-fl->fx (unsafe-flceiling size))
                PANGO_SCALE)))

(define ~pango-metric
  (lambda [val]
    (unsafe-fl/ (unsafe-fx->fl val)
                (unsafe-fx->fl PANGO_SCALE))))

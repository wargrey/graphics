#lang racket/base

(provide (all-defined-out))

(require ffi/unsafe)
(require ffi/unsafe/define)
(require ffi/unsafe/alloc)

(require racket/draw/unsafe/cairo)
(require racket/draw/unsafe/cairo-lib)

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax-rule (_hfun spec ...) (_fun #:lock-name (or cairo-lock-name "hb-lock") spec ...))
(define-syntax-rule (_tfun spec ...) (_fun #:lock-name (or cairo-lock-name "ft-lock") spec ...))
(define-syntax-rule (_cfun spec ...) (_fun #:lock-name cairo-lock-name spec ...))

(define hbuzz-lib
  (case (system-type)
    [(unix) (ffi-lib "libharfbuzz" '("0" ""))]
    [(macosx) (ffi-lib "libharfbuzz.0.dylib")]
    [(windows) (ffi-lib "libharfbuzz-0.dll")]))

(define ftype-lib
  (case (system-type)
    [(unix) (ffi-lib "libfreetype" '("6" ""))]
    [(macosx) (ffi-lib "libfreetype.6.dylib")]
    [(windows) (ffi-lib "libfreetype-6.dll")]))

(define-ffi-definer define-hbuzz hbuzz-lib #:provide provide)
(define-ffi-definer define-ftype ftype-lib #:provide provide)
(define-ffi-definer define-cairo cairo-lib #:provide provide)

(define _FT_Library (_cpointer 'FT_Library))
(define _FT_Face (_cpointer 'FT_Face))

(define _hb_blob_t (_cpointer 'hb_blob_t))
(define _hb_font_t (_cpointer 'hb_font_t))
(define _hb_face_t (_cpointer 'hb_face_t))
(define _hb_buffer_t (_cpointer 'hb_buffer_t))

(define _cairo_font_face_t (_cpointer 'cairo_font_face_t))
(define _cairo_scaled_font_t (_cpointer 'cairo_scaled_font_t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-ftype FT_New_Face (_tfun -> _FT_Face))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-hbuzz hb_version_string (_hfun -> _string))
(define-hbuzz hb_version
  (_fun [major : (_ptr o _uint)]
        [minor : (_ptr o _uint)]
        [micro : (_ptr o _uint)]
        -> _void
        -> (+ (* major 10000)
              (* minor 100)
              micro)))

(define-hbuzz hb_ot_math_has_data (_hfun _hb_font_t -> _bool))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define-cairo cairo_ft_font_face_create_for_ft_face (_cfun _FT_Face _int -> _cairo_font_face_t) #:wrap (allocator cairo_font_face_destroy))
;(define-cairo cairo_font_face_destroy (_cfun _cairo_font_face_t -> _void) #:wrap (deallocator))
(define-cairo cairo_font_face_status (_cfun _cairo_font_face_t -> _int))
(define-cairo cairo_font_face_get_reference_count (_cfun _cairo_font_face_t -> _uint))

(define-cairo cairo_ft_scaled_font_lock_face (_cfun _cairo_scaled_font_t -> _FT_Face))
(define-cairo cairo_ft_scaled_font_unlock_face (_cfun _cairo_scaled_font_t -> _void))

(define-cairo cairo_ft_font_face_get_synthesize (_cfun _cairo_font_face_t -> _uint))
(define-cairo cairo_ft_font_face_set_synthesize (_cfun _cairo_font_face_t _uint -> _void))
(define-cairo cairo_ft_font_face_unset_synthesize (_cfun _cairo_font_face_t _uint -> _void))

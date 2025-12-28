#lang racket/base

(provide (all-defined-out))

(require ffi/unsafe)
(require ffi/unsafe/define)
(require ffi/unsafe/alloc)

(require racket/draw/unsafe/cairo)

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax-rule (_tfun spec ...)
  (_fun #:lock-name (or cairo-lock-name "ft-lock")
        #:in-original-place? #true
        spec ...))

(define _ft_pos _long)
(define _ft_f26.6 _long)

(define ftype-lib
  (case (system-type)
    [(unix) (ffi-lib "libfreetype" '("6" ""))]
    [(macosx) (ffi-lib "libfreetype.6.dylib")]
    [(windows) (ffi-lib "libfreetype-6.dll")]))

(define-ffi-definer define-ftype ftype-lib #:provide provide)

(define-cstruct _FT_Glyph_Metrics
  ([width _ft_pos]
   [height _ft_pos]
   [horiBearingX _ft_pos]
   [horiBearingY _ft_pos]
   [horiAdvance _ft_pos]
   [vertBearingX _ft_pos]
   [vertBearingY _ft_pos]
   [vertAdvance _ft_pos]))

(define _FT_Library (_cpointer 'FT_Library))
(define _FT_Face (_cpointer 'FT_Face))

(define FT_LOAD_DEFAULT #x0)
(define FT_LOAD_NO_SCALE #x1)
(define FT_LOAD_NO_HINTING #x2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-ftype FT_Init_FreeType (_tfun (_ptr o _FT_Library) -> [status : _int] -> (zero? status)))
(define-ftype FT_Done_FreeType (_fun _FT_Library -> [status : _int] -> (zero? status)))

(define-ftype FT_New_Face (_tfun _FT_Library _path _long _FT_Face -> [status : _int] -> (zero? status)))
(define-ftype FT_Done_Face (_fun _FT_Face -> [status : _int] -> (zero? status)))

(define-ftype FT_Set_Char_Size (_tfun _FT_Face _ft_f26.6 _ft_f26.6 _uint _uint -> [status : _int] -> (zero? status)))
(define-ftype FT_Load_Glyph (_fun _FT_Face _uint _int -> [status : _int] -> (zero? status)))
(define-ftype FT_Get_Glyph_Metrics (_fun _FT_Face -> _FT_Glyph_Metrics))
(define-ftype FT_Get_Postscript_Name (_fun _FT_Face -> _string))

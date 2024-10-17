#lang racket/base

(provide (all-defined-out) pi nan? infinite? sgn)
(provide (all-from-out racket/draw/unsafe/cairo racket/draw/unsafe/pango))
(provide (all-from-out ffi/unsafe racket/unsafe/ops ffi/unsafe/atomic))

(require ffi/unsafe)
(require ffi/unsafe/define)
(require ffi/unsafe/alloc)
(require ffi/unsafe/atomic)

(require racket/unsafe/ops)
(require racket/draw/unsafe/pango)
(require racket/draw/unsafe/cairo)
(require racket/draw/unsafe/cairo-lib)

(require (only-in racket/math pi nan? infinite? sgn))

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax-rule (_cfun spec ...) (_fun #:lock-name "cairo-pango-lock" spec ...))
(define-syntax-rule (_pfun spec ...) (_fun #:lock-name "cairo-pango-lock" spec ...))

(define pango-lib
  (case (system-type)
    [(unix) (ffi-lib "libpango-1.0" '("0" ""))]
    [(macosx) (ffi-lib "libpango-1.0.0.dylib")]
    [(windows) (ffi-lib "libpango-1.0-0.dll")]))

(define-ffi-definer define-cairo cairo-lib #:provide provide)
(define-ffi-definer define-pango pango-lib #:provide provide)

(define PangoContext (_cpointer 'PangoContext))
(define PangoLayout (_cpointer 'PangoLayout))
(define PangoFontDescription (_cpointer 'PangoFontDescription))
(define PangoAttribute (_cpointer 'PangoAttribute))

(define _gunichar (make-ctype _uint32 char->integer integer->char))

(define-pango pango_version_string (_cfun -> _string))
(define-cairo cairo_version_string (_cfun -> _string))

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

(define-cairo cairo_push_group (_cfun _cairo_t -> _void))
(define-cairo cairo_pop_group_to_source (_cfun _cairo_t -> _void))

(define-cairo cairo_status_to_string (_cfun _int -> _string))
(define-cairo cairo_new_sub_path (_cfun _cairo_t -> _void))
(define-cairo cairo_set_miter_limit (_cfun _cairo_t _double* -> _void))
(define-cairo cairo_surface_write_to_png (_cfun _cairo_surface_t _path -> _int))
(define-cairo cairo_pattern_create_mesh (_cfun -> _cairo_pattern_t) #:wrap (allocator cairo_pattern_destroy))
(define-cairo cairo_mesh_pattern_begin_patch (_cfun _cairo_pattern_t -> _void))
(define-cairo cairo_mesh_pattern_move_to (_cfun _cairo_pattern_t _double* _double* -> _void))
(define-cairo cairo_mesh_pattern_line_to (_cfun _cairo_pattern_t _double* _double* -> _void))
(define-cairo cairo_mesh_pattern_curve_to (_cfun _cairo_pattern_t _double* _double* _double* _double* _double* _double* -> _void))
(define-cairo cairo_mesh_pattern_set_control_point (_cfun _cairo_pattern_t _uint _double* _double* -> _void))
(define-cairo cairo_mesh_pattern_set_corner_color_rgba (_cfun _cairo_pattern_t _uint _double* _double* _double* _double* -> _void))
(define-cairo cairo_mesh_pattern_end_patch (_cfun _cairo_pattern_t -> _void))

(define-cairo cairo_recording_surface_ink_extents
  (_cfun _cairo_surface_t
         [lx : (_ptr o _double)] [ty : (_ptr o _double)] [w : (_ptr o _double)] [h : (_ptr o _double)]
         -> _void -> (values (make-rectangular lx ty) w h)))

; it only works for recording surfaces that already have an extent set
;   in which case those surfaces have limited boundary to clip resulting shapes
(define-cairo cairo_recording_surface_get_extents
  (_cfun _cairo_surface_t [box : (_ptr o _cairo_rectangle_t)]
         -> [okay? : _bool]
         -> (if (not okay?)
                (values #false 0.0 0.0)
                (values (make-rectangular (cairo_rectangle_t-x box) (cairo_rectangle_t-y box))
                        (cairo_rectangle_t-width box) (cairo_rectangle_t-height box)))))

(define _pdf_metadata (_enum '(title author subject keywords producer ctime mtime)))

(define-cairo cairo_pdf_surface_set_metadata (_cfun _cairo_surface_t _pdf_metadata _string -> _void))
(define-cairo cairo_pdf_surface_set_page_label (_cfun _cairo_surface_t _string -> _void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cairo-set-source
  (lambda [cr src]
    (cond [(struct? src) ; (rgba? src)
           (cairo_set_source_rgba cr
                                  (unsafe-struct*-ref src 0)
                                  (unsafe-struct*-ref src 1)
                                  (unsafe-struct*-ref src 2)
                                  (unsafe-struct*-ref src 3))]
          [(eq? (cpointer-tag src) 'cairo_pattern_t) (cairo_set_source cr src)]
          [else (cairo_set_source_surface cr src 0.0 0.0)])))


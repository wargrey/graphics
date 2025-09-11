#lang racket/base

(provide (all-defined-out))
(provide (all-from-out racket/draw/unsafe/cairo))

(require ffi/unsafe)
(require ffi/unsafe/define)
(require ffi/unsafe/alloc)

(require racket/draw/unsafe/cairo)
(require racket/draw/unsafe/cairo-lib)

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax-rule (_cfun spec ...) (_fun #:lock-name cairo-lock-name spec ...))

(define-ffi-definer define-cairo cairo-lib #:provide provide)

(define-cairo cairo_version_string (_cfun -> _string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-cairo cairo_push_group (_cfun _cairo_t -> _void))
(define-cairo cairo_pop_group_to_source (_cfun _cairo_t -> _void))

(define-cairo cairo_status_to_string (_cfun _int -> _string))
(define-cairo cairo_new_sub_path (_cfun _cairo_t -> _void))
(define-cairo cairo_set_miter_limit (_cfun _cairo_t _double* -> _void))
(define-cairo cairo_surface_write_to_png (_cfun _cairo_surface_t _path -> _int))
(define-cairo cairo_pattern_set_extend (_cfun _cairo_pattern_t _int -> _void))
(define-cairo cairo_pattern_create_mesh (_cfun -> _cairo_pattern_t) #:wrap (allocator cairo_pattern_destroy))
(define-cairo cairo_mesh_pattern_begin_patch (_cfun _cairo_pattern_t -> _void))
(define-cairo cairo_mesh_pattern_move_to (_cfun _cairo_pattern_t _double* _double* -> _void))
(define-cairo cairo_mesh_pattern_line_to (_cfun _cairo_pattern_t _double* _double* -> _void))
(define-cairo cairo_mesh_pattern_curve_to (_cfun _cairo_pattern_t _double* _double* _double* _double* _double* _double* -> _void))
(define-cairo cairo_mesh_pattern_set_control_point (_cfun _cairo_pattern_t _uint _double* _double* -> _void))
(define-cairo cairo_mesh_pattern_set_corner_color_rgba (_cfun _cairo_pattern_t _uint _double* _double* _double* _double* -> _void))
(define-cairo cairo_mesh_pattern_end_patch (_cfun _cairo_pattern_t -> _void))

; it only works for recording surfaces that already have an extent set
;   in which case those surfaces have limited boundary to clip resulting shapes
(define-cairo cairo_recording_surface_get_extents
  (_cfun _cairo_surface_t [box : (_ptr o _cairo_rectangle_t)]
         -> [okay? : _bool]
         -> (if (not okay?)
                (values #false 0.0 0.0 0.0)
                (values (cairo_rectangle_t-x box) (cairo_rectangle_t-y box)
                        (cairo_rectangle_t-width box) (cairo_rectangle_t-height box)))))

(define _pdf_metadata (_enum '(title author subject keywords producer ctime mtime)))

(define-cairo cairo_pdf_surface_set_metadata (_cfun _cairo_surface_t _pdf_metadata _string -> _void))
(define-cairo cairo_pdf_surface_set_page_label (_cfun _cairo_surface_t _string -> _void))

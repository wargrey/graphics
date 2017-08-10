#lang racket/base

(provide (all-defined-out) create-bitmap pi nan? infinite? sgn)
(provide (all-from-out racket/draw/unsafe/cairo racket/draw/unsafe/pango))
(provide (all-from-out ffi/unsafe racket/unsafe/ops))

(require ffi/unsafe)
(require ffi/unsafe/define)
(require ffi/unsafe/alloc)

(require racket/unsafe/ops)
(require racket/draw/unsafe/pango)
(require racket/draw/unsafe/cairo)
(require racket/draw/unsafe/cairo-lib)

(require (only-in racket/math pi nan? infinite? sgn))
(require "convert.rkt")

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

(define-pango pango_context_set_font_description (_pfun PangoContext PangoFontDescription -> _void))
(define-pango pango_font_description_get_size (_pfun PangoFontDescription -> _int))
(define-pango pango_font_description_set_stretch (_pfun PangoFontDescription _int -> _void))
(define-pango pango_attr_strikethrough_new (_pfun _bool -> PangoAttribute))
(define-pango pango_attr_underline_new (_pfun _int -> PangoAttribute))

(define-pango pango_layout_get_size (_pfun PangoLayout [w : (_ptr o _int)] [h : (_ptr o _int)] -> _void -> (values w h)))
(define-pango pango_layout_set_indent (_pfun PangoLayout _int -> _void))
(define-pango pango_layout_set_spacing (_pfun PangoLayout _int -> _void))
(define-pango pango_layout_set_width (_pfun PangoLayout _int -> _void))
(define-pango pango_layout_set_height (_pfun PangoLayout _int -> _void))
(define-pango pango_layout_set_wrap (_pfun PangoLayout _int -> _void))
(define-pango pango_layout_set_ellipsize (_pfun PangoLayout _int -> _void))

(define-cairo cairo_version (_cfun -> _int))
(define-cairo cairo_version_string (_cfun -> _string))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cairo-create-argb-image
  (lambda [flwidth flheight density scale?]
    (define-values (surface cr width height) (cairo-create-argb-image* flwidth flheight density scale?))
    (values surface cr)))

(define cairo-create-argb-image*
  (lambda [flwidth flheight density scale?]
    (define width (unsafe-fxmax (~fx (unsafe-fl* flwidth density)) 1))
    (define height (unsafe-fxmax (~fx (unsafe-fl* flheight density)) 1))
    (define surface (cairo_image_surface_create CAIRO_FORMAT_ARGB32 width height))
    (define status (cairo_surface_status surface))

    (unless (unsafe-fx= status CAIRO_STATUS_SUCCESS)
      (raise-arguments-error (or (let use-next-id ([stacks (reverse (continuation-mark-set->context (current-continuation-marks)))])
                                   (and (pair? stacks)
                                        (let ([stack (unsafe-car (unsafe-car stacks))])
                                          (or (and (symbol? stack)
                                                   (let ([$stack (symbol->string stack)])
                                                     (and (unsafe-fx> (string-length $stack) 6)
                                                          (string=? (substring $stack 0 6) "bitmap")))
                                                   stack)
                                              (use-next-id (unsafe-cdr stacks))))))
                                 'make-image)
                             (cairo_status_to_string status)
                             "width" flwidth "height" flheight
                             "density" density))
    
    (define cr (cairo_create surface))
    (unless (or (not scale?) (unsafe-fl= density 1.0))
      (cairo_scale cr density density))
    (values surface cr width height)))

(define make-cairo-image
  (lambda [flwidth flheight density scale?]
    (define-values (surface cr width height) (cairo-create-argb-image* flwidth flheight density scale?))
    (values (create-bitmap surface width height density)
            cr)))

(define make-cairo-image*
  (lambda [flwidth flheight background density scale?]
    (define-values (img cr) (make-cairo-image flwidth flheight density scale?))
    (cairo-set-source cr background)
    (cairo_paint cr)
    (values img cr)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ~fx
  (lambda [fl]
    (unsafe-fl->fx (unsafe-flceiling fl))))

(define ~size
  (lambda [size]
    (unsafe-fx* (~fx size)
                PANGO_SCALE)))

(define ~metric
  (lambda [val]
    (unsafe-fl/ (unsafe-fx->fl val)
                (unsafe-fx->fl PANGO_SCALE))))

(define ~radian
  (lambda [degree]
    (unsafe-fl* degree
                (unsafe-fl/ pi 180.0))))

(define ~length
  (lambda [% 100%]
    (define fl% (real->double-flonum %))
    (cond [(flonum? %) %]
          [(single-flonum? %) (unsafe-fl* (real->double-flonum %) 100%)]
          [else (real->double-flonum %)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-values (A R G B) (if (system-big-endian?) (values 0 1 2 3) (values 3 2 1 0)))
(define-values (-pi/2 pi/2 3pi/2 2pi) (values (~radian -90.0) (~radian 90.0) (~radian 270.0) (unsafe-fl* pi 2.0)))
(define-values (the-surface the-cairo) (cairo-create-argb-image 1.0 1.0 1.0 #false))

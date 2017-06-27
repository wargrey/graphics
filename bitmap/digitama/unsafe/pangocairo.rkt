#lang racket/base

(provide (all-defined-out) make-object send is-a? pi nan? infinite?)
(provide (all-from-out racket/draw/private/bitmap))
(provide (all-from-out ffi/unsafe racket/unsafe/ops))
(provide (all-from-out racket/draw/unsafe/pango racket/draw/unsafe/cairo))

(require ffi/unsafe)
(require ffi/unsafe/define)
(require ffi/unsafe/alloc)

(require racket/unsafe/ops)
(require racket/draw/unsafe/pango)
(require racket/draw/unsafe/cairo)
(require racket/draw/unsafe/cairo-lib)
(require racket/draw/private/bitmap)

(require (only-in racket/class make-object send is-a?))
(require (only-in racket/math pi nan? infinite?))

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
(define-cairo cairo_new_sub_path (_cfun _cairo_t -> _void))
(define-cairo cairo_set_miter_limit (_cfun _cairo_t _double* -> _void))
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
    (define surface
      (cairo_image_surface_create CAIRO_FORMAT_ARGB32
                                  (unsafe-fxmax (~fx (unsafe-fl* flwidth density)) 1)
                                  (unsafe-fxmax (~fx (unsafe-fl* flheight density)) 1)))
    
    (define cr (cairo_create surface))
    (unless (or (not scale?) (unsafe-fl= density 1.0))
      (cairo_scale cr density density))
    (values surface cr)))

(define make-cairo-image
  (lambda [flwidth flheight density scale?]
    (define width (unsafe-fxmax (~fx flwidth) 1))
    (define height (unsafe-fxmax (~fx flheight) 1))
    (define img (make-bitmap width height #:backing-scale density))
    (define cr (cairo_create (send img get-handle)))
    (unless (or (not scale?) (unsafe-fl= density 1.0))
      (cairo_scale cr density density))
    (values img cr width height)))

(define make-cairo-image*
  (lambda [flwidth flheight background density scale?]
    (define-values (img cr width height) (make-cairo-image flwidth flheight density scale?))
    (cairo-set-source cr background)
    (cairo_paint cr)
    (values img cr width height)))

(define cairo-set-source
  (lambda [cr src]
    (cond [(struct? src) ; (rgba? src)
           (cairo_set_source_rgba cr
                                  (unsafe-struct*-ref src 0)
                                  (unsafe-struct*-ref src 1)
                                  (unsafe-struct*-ref src 2)
                                  (unsafe-struct*-ref src 3))]
          [(cpointer? src)
           (case (cpointer-tag src)
             [(cairo_pattern_t) (cairo_set_source cr src)]
             [(cairo_surface_t) (cairo_set_source_surface cr src 0.0 0.0)]
             [else (cairo-warn-message src "unrecognized source pointer: ~a" (cpointer-tag src))])]
          [else (cairo-warn-message src "unrecognized source type: ~a" src)])))

(define cairo-image-size
  (lambda [src density]
    (values (unsafe-fl/ (unsafe-fx->fl (cairo_image_surface_get_width src)) density)
            (unsafe-fl/ (unsafe-fx->fl (cairo_image_surface_get_height src)) density))))

(define cairo-surface-data
  (lambda [src]
    (define data (cairo_image_surface_get_data src))
    (values data (unsafe-bytes-length data))))

(define cairo-surface-metrics
  (lambda [src components]
    (define-values (data total) (cairo-surface-data src))
    (define stride (cairo_image_surface_get_stride src))
    (values data total stride
            (unsafe-fxquotient stride components)
            (unsafe-fxquotient total stride))))

(define cairo-image->bitmap
  (lambda [cr flwidth flheight density]
    (define-values (width height) (values (unsafe-fxmax (~fx flwidth) 1) (unsafe-fxmax (~fx flheight) 1)))
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
(define ~fx
  (lambda [fl]
    (unsafe-fl->fx (round fl))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-values (A R G B) (if (system-big-endian?) (values 0 1 2 3) (values 3 2 1 0)))
(define-values (-pi/2 pi/2 3pi/2 2pi) (values (~radian -90.0) (~radian 90.0) (~radian 270.0) (unsafe-fl* pi 2.0)))
(define-values (the-surface the-cairo) (cairo-create-argb-image 1.0 1.0 1.0 #false))
